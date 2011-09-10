#lang racket

(provide parse-program parse)
(require "utilities.rkt" "analysis.rkt" "env.rkt" "ast.rkt")

(define (parse-program lst env)
  (define exprs (parse-top-list (append lst '((#%halt))) env))
  (let ([r (make-seq #f exprs)])
    (for ([x (in-list exprs)]) (set-node-parent! x r))
    r))

(define (parse-top-list lst env)
  (if (pair? lst)
      (append (parse-top (car lst) env)
              (parse-top-list (cdr lst) env))
      '()))

(define (parse-top expr env)
  (match expr
    [(cons 'begin body)
     (parse-top-list body env)]
    [(list-rest 'define (list-rest var params) body)
     (parse-define var `(lambda ,params ,@body) env)]
    [(list 'define (? symbol? var) val)
     (parse-define
      var val env
      ;; If we're not defining a function, forward references are
      ;; invalid.
      (match val [`(lambda . ,rest) #t] [_ #f]))]
    [_
     (list (parse 'value expr env))]))

(define (parse-define var val env [forward-references? #t])
  (let ([var2 (env-lookup env var)])
    (parameterize ([allow-forward-references? forward-references?])
      (let* ([val2 (parse 'value val env)]
             [r    (make-def #f (list val2) var2)])
        (set-node-parent! val2 r)
        (set-var-defs! var2 (cons r (var-defs var2)))
        (list r)))))

(define (parse use expr env)
  (match expr
    [(? self-eval? expr)
     (make-cst #f '() expr)]
    [(? symbol? expr)
     (let* ([var (env-lookup env expr)]
            [r   (make-ref #f '() var)])
       (set-var-refs! var (cons r (var-refs var)))
       (if (not (var-global? var))
           (let* ([unbox (parse 'value '#%unbox env)]
                  [app (make-call #f (list unbox r))])
             (set-node-parent! r app)
             (set-node-parent! unbox app)
             app)
           r))]
    [`(set! ,lhs ,rhs)
     (let ([var (env-lookup env lhs)]
           [val (parse 'value rhs env)])
       (if (var-global? var)
           (let ([r   (make-set #f (list val) var)])
             (set-node-parent! val r)
             (set-var-sets! var (cons r (var-sets var)))
             r)
           (let* ([ref  (make-ref #f '() var)]
                  [bs   (make-ref #f '() (env-lookup env '#%box-set!))]
                  [r    (make-call #f `(,bs ,ref ,val))])
             (set-node-parent! val r)
             (set-node-parent! ref r)
             (set-node-parent! bs r)
             (set-var-sets! var (cons r (var-sets var)))
             r)))]
    [`(quote ,datum)
     (make-cst #f '() datum)]
    [`(if ,tst ,thn ,els ...)
     (let* ([a (parse 'test tst env)]
            [b (parse use thn env)]
            [c (if (null? (cdddr expr))
                   (make-cst #f '() #f)
                   (parse use (cadddr expr) env))]
            [r (make-if* #f (list a b c))])
       (set-node-parent! a r)
       (set-node-parent! b r)
       (set-node-parent! c r)
       r)]
    [`(cond . ,body) ; should eventually be a macro
     (match body
       ['()
        (parse use '(if #f #f) env)]
       [`((else . ,rhs))
        (parse use `(begin ,@rhs) env)]
       [`((,tst => ,rhs) . ,other-clauses)
        (let ([x (gensym)])
          (parse use
                 `(let ([,x ,tst])
                    (if ,x
                        (,rhs ,x)
                        (cond ,@other-clauses)))
                 env))]
       [`((,tst . ,rhs) . ,other-clauses)
        (parse use
               `(if ,tst
                    (begin ,@rhs)
                    (cond ,@other-clauses))
               env)])]
    [`(lambda ,pattern . ,body)
     (let* ([ids (extract-ids pattern)]
            ;; parent children params rest? entry-label
            [r (make-prc #f '() #f (has-rest-param? pattern) #f)]
            [new-env (env-extend env ids r)]
            [body (parse-body body new-env)]
            [mut-vars (append-map (lambda (id)
                                    (let ([v (env-lookup new-env id)])
                                      (if (mutable-var? v) (list v) '())))
                                  ids)])
       (cond [(null? mut-vars)
              (set-prc-params! r
                               (map (lambda (id) (env-lookup new-env id))
                                    ids))
              (set-node-children! r (list body))
              (set-node-parent! body r)
              r]
             [else
              (let* ([prc (make-prc #f (list body) mut-vars #f #f)]
                     [new-vars (map var-id mut-vars)]
                     [tmp-env (env-extend env new-vars r)]
                     [app
                      (make-call
                       r
                       (cons prc
                             (map (lambda (id)
                                    (parse 'value
                                           (cons '#%box (cons id '()))
                                           tmp-env))
                                  new-vars)))])
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
                r)]))]
    [`(letrec ((,ks ,vs) ...) . ,body)
     (parse use
            `(let ,(map (lambda (k) (list k #f)) ks)
               ,@(append (map (lambda (k v) `(set! ,k ,v))
                              ks vs) ; letrec*
                         body))
            env)]
    [`(begin . ,forms)
     (let* ([exprs (map (lambda (x) (parse 'value x env)) forms)]
            [r     (make-seq #f exprs)])
       (for-each (lambda (x) (set-node-parent! x r)) exprs)
       r)]
    [`(let ,(? symbol? id) ((,ks ,vs) ...) . ,body) ; named let
     (parse use
            `(letrec ([,id (lambda ,ks ,@body)])
               (,id ,@vs))
            env)]
    [`(let ((,ks ,vs) ...) . ,body)
     (parse use `((lambda ,ks ,@body) ,@vs) env)]
    [`(let* () . ,body) ; base case for let*
     (parse use `(let () ,@body) env)]
    [`(let* ((,k ,v) ,bindings ...) . ,body)
     (parse use
            `(let ([,k ,v])
               (let* ,bindings
                 ,@body))
            env)]
    ['(and) ; base case for and
     (parse use #t env)]
    [`(and ,tst)
     (parse use tst env)]
    [`(and ,tst . ,rest)
     (parse use `(if ,tst (and ,@rest) #f) env)]
    ['(or) ; base case for or
     (parse use #f env)]
    [`(or ,tst)
     (parse use tst env)]
    [`(or ,tst . ,rest)
     (if (eq? use 'test)
         ;; we don't need to keep the actual result, we only care about
         ;; its "truthiness"
         (parse use `(if ,tst #t (or ,@rest)) env)
         (parse use
                (let ([v (gensym)])
                  `(let ([,v ,tst])
                     (if ,v ,v (or ,@rest))))
                env))]
    [`(,op . ,args) (=> fail!)
     (if (memq (car expr)
               '(quote quasiquote unquote unquote-splicing lambda if set! cond
                       and or case let let* letrec begin do define delay))
         (compiler-error "the compiler does not implement the special form"
                         (car expr))
         (fail!))]
    [(? list? expr) ; call
     (let* ([exprs (map (lambda (x) (parse 'value x env)) expr)]
            [r     (make-call #f exprs)])
       (for-each (lambda (x) (set-node-parent! x r)) exprs)
       r)]
    [_
     (compiler-error "unknown expression" expr)]))

(define (parse-body exprs env)
  (parse 'value (cons 'begin exprs) env))
