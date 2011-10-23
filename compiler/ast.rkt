#lang racket

(provide (all-defined-out))
(require syntax/parse)
(require "utilities.rkt" "env.rkt")

;; Syntax-tree node representation.

(define-struct node (parent children) #:mutable #:transparent)

(define (child1 node) (car   (node-children node)))
(define (child2 node) (cadr  (node-children node)))
(define (child3 node) (caddr (node-children node)))

(define-struct (cst node) (val))
(define-struct (ref node) (var) #:mutable)
(define-struct (def node) (var)) ; children: (rhs)
(define-struct (set node) (var)) ; children: (rhs)
(define-struct (if* node) ())    ; children: (test then else)
(define-struct (prc node)        ; children: (body)
  ((params  #:mutable)
   rest?
   (entry-label #:mutable)))
(define-struct (call node) ())   ; children: (op . args)
(define-struct (seq  node) ())   ; children: (body ...)


;; parsing helpers
(define (extract-ids pattern)
  (syntax-parse pattern
    [(x:identifier ...)                #'(x ...)]
    [(x:identifier ... . y:identifier) #'(x ... y)]))
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
