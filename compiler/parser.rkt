#lang racket

(provide parse-program)
(require "utilities.rkt" "analysis.rkt" "env.rkt" "ast.rkt")
(require syntax/parse unstable/syntax)

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
  (syntax-parse expr
    ;; As in the reader, this is a hack. The Racket expander will eventually
    ;; take care of begin, define, etc. and spit out core forms.
    [(begin body ...) ; splicing begins
     #:when (eq? (syntax->datum #'begin) 'begin)
     (parse-top-list #'(body ...) env)]
    [(define (var params ...) body ...)
     #:when (eq? (syntax->datum #'define) 'define)
     (parse-define #'var #'(lambda (params ...) body ...) env)]
    [(define var:identifier val)
     #:when (eq? (syntax->datum #'define) 'define)
     (parse-define #'var #'val env
      ;; If we're not defining a function, forward references are
      ;; invalid.
      (syntax-parse #'val
        [(lambda etc ...)
         #:when (eq? (syntax->datum #'lambda) 'lambda)
         #t]
        [_ #f]))]
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
    [expr
     #:when (self-eval? (syntax->datum #'expr))
     (make-cst #f '() (syntax->datum #'expr))]
    [expr:identifier
     (let ()
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
           r))]
    [(set! lhs rhs)
     ;; Again, hack.
     #:when (eq? (syntax->datum #'set!) 'set!)
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
    [(quote* datum)
     #:when (eq? (syntax->datum #'quote*) 'quote)
     (make-cst #f '() (syntax->datum #'datum))]
    [(if* tst thn els ...)
     #:when (eq? (syntax->datum #'if*) 'if)
     (let* ([a (parse 'test #'tst env)]
            [b (parse use #'thn env)]
            [c (if (null? (syntax->list #'(els ...)))
                   (make-cst #f '() #f)
                   (parse use (car (syntax-e #'(els ...))) env))]
            [r (make-if* #f (list a b c))])
       (fix-children-parent! r)
       r)]
    [(cond body ...) ; should eventually be a macro
     #:when (eq? (syntax->datum #'cond) 'cond)
     (syntax-parse #'(body ...)
       [()
        (parse use #'(if #f #f) env)]
       [((else rhs ...))
        #:when (eq? (syntax->datum #'else) 'else)
        (parse use #'(begin rhs ...) env)]
       [((tst => rhs) other-clauses ...)
        #:when (eq? (syntax->datum #'=>) '=>)
        (let ([x (datum->syntax #'here (gensym))])
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
    [(lambda* pattern body* ...)
     #:when (eq? (syntax->datum #'lambda*) 'lambda)
     (let* ([ids (extract-ids #'pattern)]
            ;; parent children params rest? entry-label
            [r (make-prc #f '() #f (has-rest-param? #'pattern) #f)]
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
              (let* ([prc (make-prc #f (list body) mut-vars #f #f)]
                     [new-vars (map var-id mut-vars)]
                     [tmp-env (env-extend env new-vars r)]
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
     #:when (eq? (syntax->datum #'letrec) 'letrec)
     (parse use
            #'(let ([ks #f] ...)
                (set! ks vs) ...
                body ...)
            env)]
    [(begin forms ...)
     #:when (eq? (syntax->datum #'begin) 'begin)
     (let* ([exprs (syntax-map (lambda (x) (parse 'value x env)) #'(forms ...))]
            [r     (make-seq #f exprs)])
       (fix-children-parent! r)
       r)]
    [(let id:identifier ((ks vs) ...) body ...) ; named let
     #:when (eq? (syntax->datum #'let) 'let)
     (parse use
            #'(letrec ([id (lambda (ks ...) body ...)])
                (id vs ...))
            env)]
    [(let () body ...)
     #:when (eq? (syntax->datum #'let) 'let)
     (parse use #'(begin body ...) env)]
    [(let ((ks vs) ...) body ...)
     #:when (eq? (syntax->datum #'let) 'let)
     (parse use #'((lambda (ks ...) body ...) vs ...) env)]
    [(let* () body ...) ; base case for let*
     #:when (eq? (syntax->datum #'let*) 'let*)
     (parse use #'(let () body ...) env)]
    [(let* ((k v) bindings ...) body ...)
     #:when (eq? (syntax->datum #'let*) 'let*)
     (parse use
            #'(let ([k v])
                (let* (bindings ...)
                  body ...))
            env)]
    [(and)
     #:when (eq? (syntax->datum #'and) 'and)
     (parse use #'#t env)]
    [(and tst)
     #:when (eq? (syntax->datum #'and) 'and)
     (parse use #'tst env)]
    [(and tst rest ...)
     #:when (eq? (syntax->datum #'and) 'and)
     (parse use #'(if tst (and rest ...) #f) env)]
    [(or) ; base case for or
     #:when (eq? (syntax->datum #'or) 'or)
     (parse use #'#f env)]
    [(or tst)
     #:when (eq? (syntax->datum #'or) 'or)
     (parse use #'tst env)]
    [(or tst rest ...)
     #:when (eq? (syntax->datum #'or) 'or)
     (if (eq? use 'test)
         ;; we don't need to keep the actual result, we only care about
         ;; its "truthiness"
         (parse use #'(if tst #t (or rest ...)) env)
         (parse use
                (let ([v (datum->syntax #'here (gensym))])
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
             (syntax-map (lambda (e) (parse 'value e env))
                         #'(args ...))))
     (define r (make-call #f exprs))
     (fix-children-parent! r)
     r]
    [_
     (compiler-error "unknown expression" expr)]))

(define (parse-body exprs env)
  (parse 'value #`(begin #,@exprs) env))
