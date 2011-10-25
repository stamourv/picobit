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
     (var-bare-id var)]
    [(def _ `(,rhs) var)
     (list 'define (var-bare-id var) (node->expr rhs))]
    [(set _ `(,rhs) var)
     (list 'set!   (var-bare-id var) (node->expr rhs))]
    [(if* _ `(,tst ,thn ,els))
     (list 'if (node->expr tst) (node->expr thn) (node->expr els))]
    [(prc _ `(,body) params rest? entry-label)
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
