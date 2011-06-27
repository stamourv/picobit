#lang racket

(require "utilities.rkt" "ast.rkt" "env.rkt")

(provide node->expr)

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
