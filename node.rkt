#lang racket

(provide (all-defined-out))
(require racket/list) ;; take
(require "utilities.rkt" "env.rkt")

;; Syntax-tree node representation.

(define (self-eval? expr)
  (or (number?  expr)
      (char?    expr)
      (boolean? expr)
      (string?  expr)))

(define-struct node (parent children) #:mutable)

(define (child1 node) (car   (node-children node)))
(define (child2 node) (cadr  (node-children node)))
(define (child3 node) (caddr (node-children node)))

(define-struct (cst node) (val))
(define-struct (ref node) (var))
(define-struct (def node) (var))
(define-struct (set node) (var))
(define-struct (if  node) ())
(define-struct (prc node) 
  ((params  #:mutable)
   rest?
   (entry-label #:mutable)))
(define-struct (call node) ())
(define-struct (seq  node) ())
(define-struct (fix  node) (vars))

(define (node->expr node)
  (cond ((cst? node)
         (let ((val (cst-val node)))
           (if (self-eval? val)
               val
               (list 'quote val))))
        ((ref? node)
         (var-id (ref-var node)))
        ((def? node)
         (list 'define
               (var-id (def-var node))
               (node->expr (child1 node))))
        ((set? node)
         (list 'set!
               (var-id (set-var node))
               (node->expr (child1 node))))
        ((if? node)
         (list 'if
               (node->expr (child1 node))
               (node->expr (child2 node))
               (node->expr (child3 node))))
        ((prc? node)
         (if (seq? (child1 node))
             (cons 'lambda
                   (cons (build-pattern (prc-params node) (prc-rest? node))
                         (nodes->exprs (node-children (child1 node)))))
             (list 'lambda
                   (build-pattern (prc-params node) (prc-rest? node))
                   (node->expr (child1 node)))))
        ((call? node)
         (map node->expr (node-children node)))
        ((seq? node)
         (let ((children (node-children node)))
           (cond ((null? children)
                  '(void))
                 ((null? (cdr children))
                  (node->expr (car children)))
                 (else
                  (cons 'begin
                        (nodes->exprs children))))))
        ((fix? node)
         (let ((children (node-children node)))
           (list 'letrec
                 (map (lambda (var val)
                        (list (var-id var)
                              (node->expr val)))
                      (fix-vars node)
                      (take (- (length children) 1) children))
                 (node->expr (list-ref children (- (length children) 1))))))
        (else
         (compiler-error "unknown expression type" node))))

(define (nodes->exprs nodes)
  (if (null? nodes)
      '()
      (if (seq? (car nodes))
          (append (nodes->exprs (node-children (car nodes)))
                  (nodes->exprs (cdr nodes)))
          (cons (node->expr (car nodes))
                (nodes->exprs (cdr nodes))))))

(define (build-pattern params rest?)
  (cond ((null? params)
         '())
        ((null? (cdr params))
         (if rest?
             (var-id (car params))
             (list (var-id (car params)))))
        (else
         (cons (var-id (car params))
               (build-pattern (cdr params) rest?)))))
