#lang racket

(provide (all-defined-out))
(require syntax/parse unstable/match)
(require "utilities.rkt" "env.rkt")

;; Syntax-tree node representation.

(define-struct node (parent children) #:mutable #:transparent)

(define (child1 node) (car   (node-children node)))

;; If v is defined, return the node corresponding to its value.
;; Caller needs to check that v is immutable.
;; Returns #f if something goes wrong.
(define (var-val v)
  (define def (var-def v))
  (and def (child1 def)))


(define-struct (cst node) (val))
(define-struct (ref node) (var) #:mutable)
(define-struct (def node) (var)) ; children: (rhs)
(define-struct (set node) (var)) ; children: (rhs)
(define-struct (if* node) ())    ; children: (test then else)
(define-struct (prc node)        ; children: (body)
  ((params  #:mutable) ; listof var?
   rest?
   (entry-label #:mutable)))
(define-struct (call node) ())   ; children: (op . args)
(define-struct (seq  node) ())   ; children: (body ...)


;; parsing helpers
(define (extract-ids pattern)
  (syntax->list
   (syntax-parse pattern
     [(x:identifier ...)                #'(x ...)]
     [(x:identifier ... . y:identifier) #'(x ... y)])))
(define (has-rest-param? pattern)
  (syntax-parse pattern
    [(x:identifier ...)                #f]
    [(x:identifier ... . y:identifier) #t]))

;; AST construction helpers
(define (create-ref v)
  (define r (make-ref #f '() v)) ; parent needs to be set by caller
  (set-var-refs! v (cons r (var-refs v)))
  r)
(define (fix-children-parent! p)
  (for-each (lambda (x) (set-node-parent! x p)) (node-children p)))

(define (substitute-child! parent old new)
  (define children (node-children parent))
  (unless (memq old children)
    (compiler-error "substitute-child!: old is not in children"))
  (set-node-parent! new parent)
  (set-node-parent! old #f) ; just to be on the safe side
  (when (ref? old) ; remove dangling ref
    (define var (ref-var old))
    (set-var-refs! var (remq old (var-refs var))))
  (set-node-children! parent (map (lambda (x) (if (eq? x old) new x))
                                  children)))

;; Capture-avoiding substitution.
(define (substitute! e old new)
  (define (recur) (for ([c (in-list (node-children e))])
                    (substitute! c old new)))
  (match e
    [(ref p cs var) (=> fail!)
     ;; variable references don't _need_ to be eq? to be the same
     (cond [(and (ref? old) (var=? var (ref-var old)))
            (define old-var (ref-var old))
            ;; discard dangling reference, to avoid losing precision later
            (set-var-refs! old-var (remq e (var-refs old-var)))
            (substitute-child! p e (copy-node new))] ; maybe multiple old, copy
           [else (fail!)])]
    [(and (node p cs) (== old eq?)) ; eq? is used because of cycles
     (substitute-child! p e new)] ; there's only one of e, no need to copy
    [(prc p cs params rest? entry) ; the eq? case has already been handled
     (define shadowed?
       (and (ref? old) ; if it's not a ref, we use eq?, so no danger
            (for/or ([p (in-list params)]) (var=? p (ref-var old)))))
     (when (not shadowed?) (recur))]
    [(node p cs)
     (recur)]))


;; Since nodes know their parents, we can't just reuse them directly.
;; For this reason, this is a deep copy.
(define (copy-node e)
  (define new
    (match e
      [(cst p cs val) ; no need to copy val
       (make-cst #f '() val)]
      [(ref p cs var) ; no need to copy var
       (create-ref var)] ; registers the reference
      [(def p cs var) ; only at the top-level, makes no sense to copy
       (compiler-error "copying the definition of" (var-id var))]
      [(set p cs var) ; no need to copy var
       (make-set #f '() var)]
      [(if* p cs)
       (make-if* #f '())]
      [(prc p cs params rest? entry)
       (define new (make-prc #f '() '() rest? entry))
       ;; we need to create new parameters, and replace the old ones in body
       ;; Note: with Racket identifiers being used for variables, we'll need
       ;; to freshen the new vars, otherwise the new ones will be
       ;; free-identifier=? with the old ones, and we don't want that!
       (define (copy-var v) (make-local-var (var-id v) new))
       (set-prc-params! new (map copy-var params))
       ;; param substitution below
       new]
      [(call p cs)
       (make-call #f '())]
      [(seq p cs)
       (make-seq #f '())]))
  ;; parent is left #f, caller must set it
  (set-node-children!   new (map copy-node (node-children e)))
  (fix-children-parent! new)
  (when (prc? new) ; do the param substitution in body
    (for ([o (prc-params e)]
          [n (prc-params new)])
      (substitute! (car (node-children new))
                   (make-ref #f '() o)    ; fake reference, don't register
                   ;; registration will happen when substitute! calls copy-node
                   (make-ref #f '() n))))
  new)
