#lang racket

(require "utilities.rkt" "ast.rkt" "env.rkt" "analysis.rkt")

;; Front-end code transformations.

;; Note: All optimizations should be careful not to increase code size.

;------------------------------------------------------------------------------

(provide adjust-unmutable-references!)

(define (adjust-unmutable-references! node)
  (match node
    [(call parent `(,(ref _ '() var)
                    ,(and child (ref _ '() (? immutable-var? v)))))
     (=> fail!)
     (cond [(eq? (syntax->datum (var-id var)) '#%unbox) ;; TODO check better
            (set-node-parent! child parent)
            (when parent
              (set-node-children! parent (for/list ([c (node-children parent)])
                                           (if (eq? c node) child c))))
            child]
           [else (fail!)])]
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
                       ;; below fails if no definition (e.g. primitives)
                       (cst? (var-val var)))))
    (set-var-needed?! var #t)
    (let ([val (var-val var)])
      (when (and val (side-effect-less? val))
        (mark-needed-global-vars! val)))))

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

;; Beta reduction. Side-effectful. Returns the new node if succeeds, else #f.
(define (beta! e)
  (match e
    [(call parent `(,op . ,args)) (=> fail!)
     (define proc
       (match op
         [(ref p cs (? immutable-var? (app var-val proc)))
          ;; ref to an immutable var bound to a lambda, we're generous
          proc]
         [(? prc? proc)
          proc]
         [_ (fail!)]))
     (match proc
       [(prc p `(,body) params #f entry)
        (define new
          (match body
            ;; We may be able to get rid of the begin we got from the proc body
            [(seq _ `(,c)) (copy-node c)]
            [_             (copy-node body)]))
        (unless (= (length params) (length args))
          (fail!))
        (for ([o (in-list params)]
              [n (in-list args)])
          (substitute! new
                       (make-ref #f '() o) ; fake ref, don't register
                       ;; substitute-child! clears references to old params
                       n))
        ;; Hook the new node up.
        (cond [(and (seq? parent) (seq? new)) ; splice the new begin in the old
               (set-node-children!
                parent
                (apply append
                       (for/list ([c (node-children parent)])
                         (if (eq? c e) ; we replace that
                             (node-children new)
                             (list c))))) ; keep that one
               (fix-children-parent! parent)]
              [else ; just replace the original child
               (substitute-child! parent e new)])
        ;; We return the new node.
        ;; Note: new may not actually be in the program. It may have been
        ;; spliced away in its parent.
        ;; So far, all we do with the return value of beta! is to recur on it,
        ;; which basically means exploring its children. In the splicing case,
        ;; the children will have new's parent (not new itself) as parent, so
        ;; traversing them should do the right thing. (Replacing them in place
        ;; will change new's parent's children, as it should.)
        new])]
    [_ #f])) ; invalid beta, do nothing

;-----------------------------------------------------------------------------

(provide inline-calls-to-calls!)

;; When an operator is a reference to a function whose body is just a call, we
;; may be able to inline without increasing code size.
;; All args to the inner call need to be trivial (cst or ref), and there needs
;; to be at most as many as there are arguments in the original call.
;; ex: (define (foo x y) (bar x y)) (foo 1)
;;     => (bar 1)
;; Optionally takes a list of vars seen for a given call site.
;; Every time we successfully change the operator, we add it to the list.
;; If we see something again, we're looping, so stop there.
(define (inline-calls-to-calls! node [seen '()])
  (match node
    [(call p `(,(and orig-op (ref _ '() (and orig-var (app var-val val))))
               . ,args))
     (=> fail!)
     (match val
       [(prc _ `(,body) params #f entry)
        (match body
          [(seq _ `(,(call new-p `(,(ref _ '() inside-var) . ,inside-args))))
           ;; We need to be careful to not increase code size.
           (if (and (<= (length inside-args) (length args)) ; not too many
                    (for/and ([i-a (in-list inside-args)])
                      (or (ref? i-a) (cst? i-a))) ; not too big
                    ;; Don't loop.
                    (not (for/or ([s seen]) (var=? s inside-var))))
               (let ([new (beta! node)])
                 ;; If beta fails, nothing was changed. No point in recurring,
                 ;; since trying again is useless, and all our children are
                 ;; trivial (cst or ref).
                 ;; If beta succeeds, we recur, there may be new opportunities.
                 ;; Note: new may not be a node that's actually in the program.
                 ;; See comment in beta!.
                 (when new (inline-calls-to-calls! new (cons orig-var seen))))
               (fail!))]
          [_ (fail!)])]
       [_ (fail!)])]
    [_
     (for-each inline-calls-to-calls! (node-children node))]))

;-----------------------------------------------------------------------------

(provide constant-fold!)

(define (constant-fold! node)
  (match node
    ;; if we're calling a primitive
    [(call p `(,(ref _ '() (? var-primitive op))
               . ,args))
     (=> fail!)
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
               (lambda (e) (fail!)) ; something went wrong, back off
               ;; replace the call with the constant
               (lambda ()
                 (define res-val (apply folder (map cst-val args)))
                 (define res     (make-cst p '() res-val))
                 (substitute-child! p node res)))]
             [else
              (fail!)]))]
    [_
     (for-each constant-fold! (node-children node))]))

;-----------------------------------------------------------------------------

(provide copy-propagate!)

(define (copy-propagate! expr)
  (match expr
    [(ref p cs (? immutable-var? (and var (app var-val (? values val)))))
     (=> fail!)
     (define (replace!)
       (unless (node-parent expr) (fail!)) ; no parent, stale node, ignore
       (substitute-child! p expr (copy-node val))
       (copy-propagate! p)) ;  there may be more to do, start our parent again
     (match val
       [(ref _ cs new-var)
        (replace!)]
       [(cst _ cs v)
        ;; constants are ok. even if they're large, they're just a pointer into
        ;; ROM, where the constant would have been anyway (and no duplication)
        (replace!)]
       [_ (fail!)])] ; anything else would increase code size
    [_
     (for-each copy-propagate! (node-children expr))]))
