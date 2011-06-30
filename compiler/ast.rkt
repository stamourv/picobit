#lang racket

(provide (all-defined-out))
(require racket/list) ;; take
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


(define (extract-ids pattern)
  (cond [(pair? pattern)
         (cons (car pattern) (extract-ids (cdr pattern)))]
        [(symbol? pattern)
         (cons pattern '())]
        [else
         '()]))

(define (has-rest-param? pattern)
  (if (pair? pattern)
      (has-rest-param? (cdr pattern))
      (symbol? pattern)))
