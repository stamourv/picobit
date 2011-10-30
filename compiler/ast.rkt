#lang racket

(provide (all-defined-out))
(require syntax/parse unstable/match racket/syntax)
(require "utilities.rkt" "env.rkt")

;; Syntax-tree node representation.

(define-struct node (parent children) #:mutable #:transparent)

(define (child1 node) (car   (node-children node)))

(define (immutable-var? var) (null? (var-sets var)))
(define (mutable-var?   var) (not (immutable-var? var)))

;; If v is defined, return the node corresponding to its value.
;; Returns #f if something goes wrong.
(define (var-val v)
  (define def (var-def v))
  (and (immutable-var? v)
       def
       (not (prc? def)) ; var defined in a lambda, no fixed value
       (child1 def)))   ; rhs of a define


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
;; Needs to be called every time we remove a ref from the AST to avoid
;; dangling references which hurt analysis precision.
(define (discard-ref r)
  (define var  (ref-var r))
  (define refs (var-refs var))
  (unless (memq r refs)
    (compiler-error "discard-ref: ref does not refer to the variable"))
  (set-var-refs! var (remq r refs)))
(define (fix-children-parent! p)
  (for-each (lambda (x) (set-node-parent! x p)) (node-children p)))

(define (substitute-child! parent old new)
  (define children (node-children parent))
  (unless (memq old children)
    (compiler-error "substitute-child!: old is not in children"))
  (set-node-parent! new parent)
  (set-node-parent! old #f) ; just to be on the safe side
  (when (ref? old) (discard-ref old)) ; remove dangling ref
  (set-node-children! parent (map (lambda (x) (if (eq? x old) new x))
                                  children)))

;; Capture-avoiding substitution.
(define (substitute! e old new)
  (define (recur) (for ([c (in-list (node-children e))])
                    (substitute! c old new)))
  (match e
    [(ref p _ var) (=> fail!)
     ;; variable references don't _need_ to be eq? to be the same
     (cond [(and (ref? old) (var=? var (ref-var old)))
            (substitute-child! p e (copy-node new))] ; maybe multiple old, copy
           [else (fail!)])]
    [(and (node p _) (== old eq?)) ; eq? is used because of cycles
     (substitute-child! p e new)] ; there's only one of e, no need to copy
    [(node p cs)
     (recur)]))


;; Since nodes know their parents, we can't just reuse them directly.
;; For this reason, this is a deep copy.
(define (copy-node e)
  (define new
    (match e
      ;; parent is left #f, caller must set it
      ;; children are copied below
      [(cst _ _ val) ; no need to copy val
       (make-cst #f '() val)]
      [(ref _ _ var) ; no need to copy var
       (create-ref var)] ; registers the reference
      [(def _ _ var) ; only at the top-level, makes no sense to copy
       (compiler-error "copying the definition of" (var-id var))]
      [(set _ _ var) ; no need to copy var
       (make-set #f '() var)]
      [(if* _ _)
       (make-if* #f '())]
      [(prc _ _ params rest? entry)
       (define new (make-prc #f '() '() rest? entry))
       ;; we need to create new parameters, and replace the old ones in body
       ;; Note: with Racket identifiers being used for variables, we'll need
       ;; to freshen the new vars, otherwise the new ones will be
       ;; free-identifier=? with the old ones, and we don't want that!
       (define (copy-var v)
         (make-local-var (generate-temporary (var-id v)) new))
       (define new-params (map copy-var params))
       (set-prc-params! new new-params)
       ;; param substitution below
       new]
      [(call _ _)
       (make-call #f '())]
      [(seq _ _)
       (make-seq #f '())]))
  (set-node-children! new (map copy-node (node-children e)))
  (fix-children-parent! new)
  (when (prc? new) ; do the param substitution in body
    (for ([o (prc-params e)]
          [n (prc-params new)])
      (substitute! (car (node-children new))
                   (make-ref #f '() o)    ; fake reference, don't register
                   ;; registration will happen when substitute! calls copy-node
                   (make-ref #f '() n))))
  new)


;; Pretty-printer, mostly for debugging

(provide node->expr)

(define (node->expr node)
  (match node
    [(cst _ '() val)
     (if (self-eval? val)
         val
         (list 'quote val))]
    [(ref _ '() var)
     (var-bare-id var)]
    [(def _ `(,rhs) var)
     (list 'define (var-bare-id var) (node->expr rhs))]
    [(set _ `(,rhs) var)
     (list 'set!   (var-bare-id var) (node->expr rhs))]
    [(if* _ `(,tst ,thn ,els))
     (list 'if (node->expr tst) (node->expr thn) (node->expr els))]
    [(prc _ `(,body) params rest? _)
     (define (build-pattern params rest?)
       (cond [(null? params)
              '()]
             [(null? (cdr params))
              (if rest?
                  (var-bare-id (car params))
                  (list (var-bare-id (car params))))]
             [else
              (cons (var-bare-id (car params))
                    (build-pattern (cdr params) rest?))]))
     `(lambda ,(build-pattern params rest?)
        ,(node->expr body))]
    [(call _ children)
     (map node->expr children)]
    [(seq _ children)
     (cons 'begin (map node->expr children))]
    [_
     (compiler-error "unknown expression type" node)]))
