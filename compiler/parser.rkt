#lang racket

(provide parse-program)
(require "utilities.rkt" "analysis.rkt" "env.rkt" "ast.rkt")
(require syntax/parse racket/syntax)

(define (parse-program lst env)
  (define exprs
    (append extra-code-env
            (parse-top-list #`(#,@lst (#%halt)) env)))
  (define r (make-seq #f exprs))
  (fix-children-parent! r)
  r)

(define (parse-top-list lst env)
  (append-map (lambda (e) (parse-top e env))
              (syntax->list lst)))

;; returns a list of parsed expressions
(define (parse-top expr env)
  (syntax-parse expr #:literals (define lambda)
    ;; As in the reader, this is a hack. The Racket expander will eventually
    ;; take care of begin, define, etc. and spit out core forms.
    [(begin body ...) ; splicing begins
     #:when (eq? (syntax->datum #'begin) 'begin)
     (parse-top-list #'(body ...) env)]
    [(define (var params ...) body ...)
     (parse-define #'var #'(lambda (params ...) body ...) env)]
    [(define var:identifier val)
     (parse-define #'var #'val env
      ;; If we're not defining a function, forward references are
      ;; invalid.
      (syntax-parse #'val
        [(lambda etc ...) #t]
        [_                #f]))]
    [_
     (list (parse 'value expr env))]))

(define (parse-define var val env [forward-references? #t])
  (let ([var2 (env-lookup env var)])
    (parameterize ([allow-forward-references? forward-references?])
      (let* ([val2 (parse 'value val env)]
             [r    (make-def #f (list val2) var2)])
        (fix-children-parent! r)
        (when (var-def var2)
          (compiler-error "variable redefinition forbidden" var2))
        (set-var-def! var2 r)
        (list r)))))

(define (parse use expr env [operator-position? #f])
  (syntax-parse expr
    #:literals (set! quote if cond else => lambda letrec begin let let* and or)
    [expr
     #:when (self-eval? (syntax->datum #'expr))
     (make-cst #f '() (syntax->datum #'expr))]
    [expr:identifier
     (define var
       (let* ([v    (env-lookup env #'expr)]
              [prim (var-primitive v)])
         (if (and prim (not operator-position?))
             ;; We eta-expand any primitive used in a higher-order fashion.
             (primitive-eta-expansion prim)
             v)))
     (define r (create-ref var))
     (if (not (var-global? var))
         (let* ([unbox (parse 'value #'#%unbox env)]
                [app (make-call #f (list unbox r))])
           (fix-children-parent! app)
           app)
         r)]
    [(set! lhs rhs)
     ;; Again, hack.
     (let ([var (env-lookup env #'lhs)]
           [val (parse 'value #'rhs env)])
       (when (var-primitive var)
         (compiler-error "cannot mutate primitive" (var-id var)))
       (if (var-global? var)
           (let ([r (make-set #f (list val) var)])
             (fix-children-parent! r)
             (set-var-sets! var (cons r (var-sets var)))
             r)
           (let* ([ref  (create-ref var)]
                  [bs   (create-ref (env-lookup env #'#%box-set!))]
                  [r    (make-call #f `(,bs ,ref ,val))])
             (fix-children-parent! r)
             (set-var-sets! var (cons r (var-sets var)))
             r)))]
    [(quote datum)
     (make-cst #f '() (syntax->datum #'datum))]
    [(if tst thn els ...)
     (let* ([a (parse 'test #'tst env)]
            [b (parse use #'thn env)]
            [c (if (null? (syntax->list #'(els ...)))
                   (make-cst #f '() #f)
                   (parse use (car (syntax-e #'(els ...))) env))]
            [r (make-if* #f (list a b c))])
       (fix-children-parent! r)
       r)]
    [(cond body ...) ; should eventually be a macro
     (syntax-parse #'(body ...)
       [()
        (parse use #'(if #f #f) env)]
       [((else rhs ...))
        (parse use #'(begin rhs ...) env)]
       [((tst => rhs) other-clauses ...)
        (let ([x (generate-temporary)])
          (parse use
                 #`(let ([#,x tst])
                     (if #,x
                         (rhs #,x)
                         (cond other-clauses ...)))
                 env))]
       [((tst rhs ...) other-clauses ...)
        (parse use
               #'(if tst
                     (begin rhs ...)
                     (cond other-clauses ...))
               env)])]
    [(lambda pattern body* ...)
     (let* ([ids (extract-ids #'pattern)]
            ;; children params rest?
            [r (create-prc '() #f (has-rest-param? #'pattern))]
            [new-env (env-extend env ids r)]
            [body (parse-body #'(body* ...) new-env)]
            [mut-vars (for*/list ([id (in-list ids)]
                                  [v  (in-value (env-lookup new-env id))]
                                  #:when (mutable-var? v))
                        v)])
       (cond [(null? mut-vars)
              (set-prc-params! r
                               (map (lambda (id) (env-lookup new-env id))
                                    ids))
              (set-node-children! r (list body))
              (fix-children-parent! r)
              r]
             [else
              (let* ([prc (create-prc (list body) mut-vars #f)] ; no rest
                     [new-vars (map var-id mut-vars)]
                     [tmp-env (env-extend new-env new-vars r)]
                     [app
                      (make-call
                       r
                       (cons prc
                             (map (lambda (id)
                                    (parse 'value #`(#%box #,id) tmp-env))
                                  new-vars)))])
                ;; (lambda (a b) (set! a b))
                ;; => (lambda (_a b) ((lambda (a) (box-set! a b)) (box _a)))
                (for-each (lambda (var) (set-var-def! var prc))
                          mut-vars)
                (fix-children-parent! app)
                (set-prc-params! r
                                 (map (lambda (id) (env-lookup tmp-env id))
                                      ids))
                (set-node-children! r (list app))
                (fix-children-parent! prc)
                r)]))]
    [(letrec ((ks vs) ...) body ...)
     (parse use
            #'(let ([ks #f] ...)
                (set! ks vs) ...
                body ...)
            env)]
    [(begin forms ...)
     (let ([exprs (map (lambda (x) (parse 'value x env))
                       (syntax->list #'(forms ...)))])
       (cond [(> (length exprs) 1)
              (define r (make-seq #f exprs))
              (fix-children-parent! r)
              r]
             [else
              (car exprs)]))]
    [(let id:identifier ((ks vs) ...) body ...) ; named let
     (parse use
            #'(letrec ([id (lambda (ks ...) body ...)])
                (id vs ...))
            env)]
    [(let () body ...)
     (parse use #'(begin body ...) env)]
    [(let ((ks vs) ...) body ...)
     (parse use #'((lambda (ks ...) body ...) vs ...) env)]
    [(let* () body ...) ; base case for let*
     (parse use #'(let () body ...) env)]
    [(let* ((k v) bindings ...) body ...)
     (parse use
            #'(let ([k v])
                (let* (bindings ...)
                  body ...))
            env)]
    [(and)
     (parse use #'#t env)]
    [(and tst)
     (parse use #'tst env)]
    [(and tst rest ...)
     (parse use #'(if tst (and rest ...) #f) env)]
    [(or) ; base case for or
     (parse use #'#f env)]
    [(or tst)
     (parse use #'tst env)]
    [(or tst rest ...)
     (if (eq? use 'test)
         ;; we don't need to keep the actual result, we only care about
         ;; its "truthiness"
         (parse use #'(if tst #t (or rest ...)) env)
         (parse use
                (let ([v (generate-temporary)])
                  #`(let ([#,v tst])
                      (if #,v #,v (or rest ...))))
                env))]
    [(op args ...)
     #:when (memq (syntax->datum #'op)
                  '(quote quasiquote unquote unquote-splicing lambda if set!
                    cond and or case let let* letrec begin do define delay))
     (compiler-error "the compiler does not implement the special form"
                     (syntax->datum #'op))]
    [(op args ...) ; call
     (define exprs
       (cons (parse 'value #'op env #t) ; in operator position
             (map (lambda (e) (parse 'value e env))
                  (syntax->list #'(args ...)))))
     (define r (make-call #f exprs))
     (fix-children-parent! r)
     r]
    [_
     (compiler-error "unknown expression" expr)]))

(define (parse-body exprs env)
  (parse 'value #`(begin #,@exprs) env))
