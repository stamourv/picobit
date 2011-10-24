#lang racket

(provide (all-defined-out))
(require syntax/parse unstable/match)
(require "utilities.rkt" "env.rkt")

;; Syntax-tree node representation.

(define-struct node (parent children) #:mutable #:transparent)

(define (child1 node) (car   (node-children node)))
(define (child2 node) (cadr  (node-children node)))
(define (child3 node) (caddr (node-children node)))

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
  (set-node-parent! new parent)
  (set-node-parent! old #f) ; just to be on the safe side
  (set-node-children! parent (map (lambda (x) (if (eq? x old) new x))
                                  (node-children parent))))

;; Capture-avoiding substitution.
(define (substitute! e old new)
  (define (do-it) (substitute-child! (node-parent e) e new))
  (define (recur) (for ([c (in-list (node-children e))])
                    (substitute! c old new)))
  (match e
    [(and (node p cs) (== old eq?)) ; eq? is used because of cycles
     (do-it)]
    [(ref p cs var) (=> fail!)
     ;; variable references don't _need_ to be eq? to be the same
     (if (and (ref? old) (var=? var (ref-var old)))
         (do-it)
         (fail!))]
    [(prc p cs params rest? entry) ; the eq? case has already been handled
     (define shadowed?
       (and (ref? old) ; if it's not a ref, we use eq?, so no danger
            (for/or ([p (in-list params)]) (var=? p (ref-var old)))))
     (when (not shadowed?) (recur))]
    [(node p cs)
     (recur)]))
