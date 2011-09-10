#lang racket

(require "utilities.rkt" "ast.rkt" "env.rkt" "analysis.rkt")

;; Front-end code transformations.

;------------------------------------------------------------------------------

(provide adjust-unmutable-references!)

(define (adjust-unmutable-references! node)
  (match node
    [(call parent `(,(ref _ '() (app var-id '#%unbox))
                    ,(and child (ref _ '() (? immutable-var? v)))))
     (set-node-parent! child parent)
     (when parent
       (set-node-children! parent (for/list ([c (node-children parent)])
                                    (if (eq? c node) child c))))
     child]
    [_
     (for-each adjust-unmutable-references! (node-children node))
     node]))

;-----------------------------------------------------------------------------

(provide mark-needed-global-vars!)

(define (mark-var! var)
  (when (and (var-global? var)
             (not (var-needed? var))
             ;; globals that obey the following conditions are considered
             ;; to be constants
             (not (and (not (mutable-var? var))
                       ;; to weed out primitives, which have no definitions
                       (> (length (var-defs var)) 0)
                       (cst? (child1 (car (var-defs var)))))))
    (set-var-needed?! var #t)
    (for ([def (in-list (var-defs var))])
      (let ([val (child1 def)])
        (when (side-effect-less? val)
          (mark-needed-global-vars! val))))))

(define (mark-needed-global-vars! node)
  (match node
    [(ref _ '() var)
     (mark-var! var)]
    [(def _ `(,val) var)
     (when (not (side-effect-less? val))
       (mark-needed-global-vars! val))]
    [(or (? cst? node) (? set? node) (? if*? node) (? prc? node)
         (? call? node) (? seq? node))
     (for-each mark-needed-global-vars! (node-children node))]
    [_
     (compiler-error "unknown expression type" node)]))

;-----------------------------------------------------------------------------

(provide inline-eta!)

;; When an eta-expansion is used in operator position, it can be replaced
;; by the function it's wrapping.
;; ex: (define (foo x y) (bar x y)) (foo 1)
;;     => (bar 1)
(define (inline-eta! node)
  (match node
    [(call p `(,(and orig-op (ref _ '() (app var-defs `(,d . ,rest))))
               . ,args))
     (=> unmatch)
     (match d
       [(def p `(,(prc _ `(,body) params #f entry)) v)
        (match body
          [(seq _ `(,(call p `(,(ref _ '() inside-op) . ,inside-args))))
           (if (andmap ref? inside-args)
               ;; since the call is directly inside the lambda (no
               ;; intermediate scopes), we can compare ids directly
               (let ([inside-args (map (lambda (x) (var-id (ref-var x)))
                                       inside-args)]
                     [params    (map var-id params)])
                 (cond [(equal? inside-args params)
                        ;; we can replace orig-op with inside-op
                        (set-ref-var! orig-op inside-op)]
                       [else (unmatch)]))
               (unmatch))]
          [_ (unmatch)])]
       [_ (unmatch)])]
    [_
     (for-each inline-eta! (node-children node))]))

;-----------------------------------------------------------------------------

(provide constant-fold!)

(define (constant-fold! node)
  (match node
    ;; if we're calling a primitive
    [(call p `(,(ref _ '() (? var-primitive op))
               . ,args))
     (=> unmatch)
     (for-each constant-fold! args) ; fold args before the whole call
     (let ([folder (primitive-constant-folder (var-primitive op))]
           ;; (we need to access the children again (can't just use `args',
           ;; since constant folding may have mutated them)
           [args   (cdr (node-children node))])
       ;; the primitive can do constant-folding, and the args are constant
       ;; folder takes the values of the args, and returns the value of the res
       (cond [(and folder (andmap cst? args))
              ;; if the folding would raise an error, just don't do it, and
              ;; error at runtime
              (call-with-exception-handler
               (lambda (e) (unmatch))
               (lambda ()
                 (define res-val (apply folder (map cst-val args)))
                 (define res     (make-cst p '() res-val))
                 ;; replace the call with the constant
                 (set-node-children! p (map (lambda (x)
                                              (if (eq? x node) res x))
                                            (node-children p)))))]
             [else
              (unmatch)]))]
    [_
     (for-each constant-fold! (node-children node))]))
