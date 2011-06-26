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
(define-struct (ref node) (var))
(define-struct (def node) (var))
(define-struct (set node) (var))
(define-struct (if* node) ())
(define-struct (prc node) 
  ((params  #:mutable)
   rest?
   (entry-label #:mutable)))
(define-struct (call node) ())
(define-struct (seq  node) ())

(define (node->expr node)
  (match node
    [(cst _ '() val)
     (if (self-eval? val)
         val
         (list 'quote val))]
    [(ref _ '() var)
     (var-id var)]
    [(def _ `(,rhs) var)
     (list 'define (var-id var) (node->expr rhs))]
    [(set _ `(,rhs) var)
     (list 'set!   (var-id var) (node->expr rhs))]
    [(if* _ `(,tst ,thn ,els))
     (list 'if (node->expr tst) (node->expr thn) (node->expr els))]
    [(prc _ `(,body) params rest? entry-label)
     (define (build-pattern params rest?)
       (cond [(null? params)
              '()]
             [(null? (cdr params))
              (if rest?
                  (var-id (car params))
                  (list (var-id (car params))))]
             [else
              (cons (var-id (car params))
                    (build-pattern (cdr params) rest?))]))
     `(lambda ,(build-pattern params rest?)
        ,@(if (seq? body)
              (nodes->exprs (node-children body))
              (list (node->expr body))))]
    [(call _ children)
     (map node->expr children)]
    [(seq _ children)
     (cond [(null? children)
            '(void)]
           [(null? (cdr children))
            (node->expr (car children))]
           [else
            (cons 'begin (nodes->exprs children))])]
    [_
     (compiler-error "unknown expression type" node)]))

(define (nodes->exprs nodes)
  (if (null? nodes)
      '()
      (if (seq? (car nodes))
          (append (nodes->exprs (node-children (car nodes)))
                  (nodes->exprs (cdr nodes)))
          (cons (node->expr (car nodes))
                (nodes->exprs (cdr nodes))))))

(define (extract-ids pattern)
  (if (pair? pattern)
      (cons (car pattern) (extract-ids (cdr pattern)))
      (if (symbol? pattern)
          (cons pattern '())
          '())))

(define (has-rest-param? pattern)
  (if (pair? pattern)
      (has-rest-param? (cdr pattern))
      (symbol? pattern)))
