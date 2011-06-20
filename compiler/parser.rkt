#lang racket

(provide parse-top-list parse)
(require "utilities.rkt" "analysis.rkt" "env.rkt" "ast.rkt")

(define (parse-top-list lst env)
  (if (pair? lst)
      (append (parse-top (car lst) env)
              (parse-top-list (cdr lst) env))
      '()))

(define (parse-top expr env)
  (cond ((and (pair? expr)
              (eq? (car expr) 'begin))
         (parse-top-list (cdr expr) env))
        ((and (pair? expr)
              (eq? (car expr) 'define))
         (let ((var
                (if (pair? (cadr expr))
                    (car (cadr expr))
                    (cadr expr)))
               (val
                (if (pair? (cadr expr))
                    (cons 'lambda (cons (cdr (cadr expr)) (cddr expr)))
                    (caddr expr))))
           (let* ((var2 (env-lookup env var))
                  (val2 (parse 'value val env))
                  (r (make-def #f (list val2) var2)))
             (set-node-parent! val2 r)
             (set-var-defs! var2 (cons r (var-defs var2)))
             (list r))))
        (else
         (list (parse 'value expr env)))))

(define (parse use expr env)
  (cond ((self-eval? expr)
         (make-cst #f '() expr))
        ((symbol? expr)
         (let* ((var (env-lookup env expr))
                (r (make-ref #f '() var)))
           (set-var-refs! var (cons r (var-refs var)))
           (if (not (var-global? var))
               (let* ((unbox (parse 'value '#%unbox env))
                      (app (make-call #f (list unbox r))))
                 (set-node-parent! r app)
                 (set-node-parent! unbox app)
                 app)
               r)))
        ((and (pair? expr)
              (eq? (car expr) 'set!))
         (let ((var (env-lookup env (cadr expr))))
           (if (var-global? var)
               (let* ((val (parse 'value (caddr expr) env))
                      (r (make-set #f (list val) var)))
                 (set-node-parent! val r)
                 (set-var-sets! var (cons r (var-sets var)))
                 r)
               (let* ((body (parse 'value (caddr expr) env))
                      (ref (make-ref #f '() var))
                      (bs (make-ref #f '() (env-lookup env '#%box-set!)))
                      (r (make-call #f (list bs ref body))))
                 (set-node-parent! body r)
                 (set-node-parent! ref r)
                 (set-node-parent! bs r)
                 (set-var-sets! var (cons r (var-sets var)))
                 r))))
        ((and (pair? expr)
              (eq? (car expr) 'quote))
         (make-cst #f '() (cadr expr)))
        ((and (pair? expr)
              (eq? (car expr) 'if))
         (let* ((a (parse 'test (cadr expr) env))
                (b (parse use (caddr expr) env))
                (c (if (null? (cdddr expr))
                       (make-cst #f '() #f)
                       (parse use (cadddr expr) env)))
                (r (make-if* #f (list a b c))))
           (set-node-parent! a r)
           (set-node-parent! b r)
           (set-node-parent! c r)
           r))
        ((and (pair? expr)
              (eq? (car expr) 'cond)) ; should eventually be a macro
         (let ([body (cdr expr)])
           (if (null? body)
               (parse use '(if #f #f) env)
               (cond [(eq? (caar body) 'else)
                      (parse use `(begin . ,(cdar body)) env)]
                     [(and (not (null? (cdar body)))
                           (eq? (cadar body) '=>))
                      (let ([x (gensym)])
                        (parse use
                               `(let ((,x ,(caar body)))
                                  (if ,x
                                      (,(caddar body) ,x)
                                      (cond . ,(cdr body))))
                               env))]
                     [else
                      (parse use
                             `(if ,(caar body)
                                  (begin . ,(cdar body))
                                  (cond . ,(cdr body)))
                             env)]))))
        ((and (pair? expr)
              (eq? (car expr) 'lambda))
         (let* ((pattern (cadr expr))
                (ids (extract-ids pattern))
                ;; parent children params rest? entry-label
                (r (make-prc #f '() #f (has-rest-param? pattern) #f))
                (new-env (env-extend env ids r))
                (body (parse-body (cddr expr) new-env))
                (mut-vars
                 (apply append
                        (map (lambda (id)
                               (let ((v (env-lookup new-env id)))
                                 (if (mutable-var? v) (list v) '())))
                             ids))))
           (if (null? mut-vars)
               (begin
                 (set-prc-params! r
                                  (map (lambda (id) (env-lookup new-env id))
                                       ids))
                 (set-node-children! r (list body))
                 (set-node-parent! body r)
                 r)
               (let* ((prc (make-prc #f (list body) mut-vars #f #f))
                      (new-vars (map var-id mut-vars))
                      (tmp-env (env-extend env new-vars r))
                      (app
                       (make-call
                        r
                        (cons prc
                              (map (lambda (id)
                                     (parse 'value
                                            (cons '#%box (cons id '()))
                                            tmp-env))
                                   new-vars)))))
                 ;; (lambda (a b) (set! a b))
                 ;; => (lambda (_a b) ((lambda (a) (box-set! a b)) (box _a)))
                 (for-each (lambda (var) (set-var-defs! var (list prc)))
                           mut-vars)
                 (for-each (lambda (n) (set-node-parent! n app))
                           (cdr (node-children app)))
                 (set-node-parent! prc app)
                 (set-prc-params! r
                                  (map (lambda (id) (env-lookup tmp-env id))
                                       ids))
                 (set-node-children! r (list app))
                 (set-node-parent! body prc)
                 r))))
        ((and (pair? expr)
              (eq? (car expr) 'letrec))
         (let ((ks (map car (cadr expr)))
               (vs (map cadr (cadr expr))))
           (parse use
                  (cons 'let
                        (cons (map (lambda (k) (list k #f)) ks)
                              (append (map (lambda (k v) (list 'set! k v))
                                           ks vs) ; letrec*
                                      (cddr expr))))
                  env)))
        ((and (pair? expr)
              (eq? (car expr) 'begin))
         (let* ((exprs (map (lambda (x) (parse 'value x env)) (cdr expr)))
                (r (make-seq #f exprs)))
           (for-each (lambda (x) (set-node-parent! x r)) exprs)
           r))
        ((and (pair? expr)
              (eq? (car expr) 'let))
         (if (symbol? (cadr expr))
             (parse use
                    `(letrec ((,(cadr expr) (lambda ,(map car (caddr expr)) .
                                                    ,(cdddr expr))))
                       (,(cadr expr) . ,(map cadr (caddr expr))))
                    env)
             (parse use
                    (cons (cons 'lambda
                                (cons (map car (cadr expr))
                                      (cddr expr)))
                          (map cadr (cadr expr)))
                    env)))
        ((and (pair? expr)
              (eq? (car expr) 'let*))
         (if (null? (cadr expr))
             (parse use
                    (cons 'let (cdr expr))
                    env)
             (parse use
                    (list 'let
                          (list (list (caar (cadr expr))
                                      (cadar (cadr expr))))
                          (cons 'let*
                                (cons (cdr (cadr expr))
                                      (cddr expr))))
                    env)))
        ((and (pair? expr)
              (eq? (car expr) 'and))
         (cond ((null? (cdr expr))
                (parse use
                       #t
                       env))
               ((null? (cddr expr))
                (parse use
                       (cadr expr)
                       env))
               (else
                (parse use
                       (list 'if
                             (cadr expr)
                             (cons 'and (cddr expr))
                             #f)
                       env))))
        ((and (pair? expr)
              (eq? (car expr) 'or))
         (cond ((null? (cdr expr))
                (parse use
                       #f
                       env))
               ((null? (cddr expr))
                (parse use
                       (cadr expr)
                       env))
               ((eq? use 'test)
                (parse use
                       (list 'if
                             (cadr expr)
                             #t
                             (cons 'or (cddr expr)))
                       env))
               (else
                (parse use
                       (let ((v (gensym)))
                         (list 'let
                               (list (list v (cadr expr)))
                               (list 'if
                                     v
                                     v
                                     (cons 'or (cddr expr)))))
                       env))))
        ;; primitive substitution here
        ;; TODO do this optimization in the following pass instead of at parse time ?
        ((and (pair? expr)
              (assoc (car expr) substitute-primitives))
         =>
         (lambda (prim)
           (parse use
                  (cons (cdr prim) (cdr expr))
                  env)))
        ;; binary arthimetic operations can use primitives directly
        ((and (pair? expr)
              (= (length (cdr expr)) 2)
              (assoc (car expr) '((+ . #%+) (- . #%-))))
         =>
         (lambda (prim)
           (parse use
                  (cons (cdr prim) (cdr expr))
                  env)))
        ((and (pair? expr)
              (memq (car expr)
                    '(quote quasiquote unquote unquote-splicing lambda if
                            set! cond and or case let let* letrec begin do define
                            delay)))
         (compiler-error "the compiler does not implement the special form" (car expr)))
        ((pair? expr)
         (let* ((exprs (map (lambda (x) (parse 'value x env)) expr))
                (r (make-call #f exprs)))
           (for-each (lambda (x) (set-node-parent! x r)) exprs)
           r))
        (else
         (compiler-error "unknown expression" expr))))

(define (parse-body exprs env)
  (parse 'value (cons 'begin exprs) env))
