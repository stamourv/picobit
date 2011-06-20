#lang racket

(provide adjust-unmutable-references!)

(require "ast.rkt" "env.rkt" "analysis.rkt")

;; Front-end code transformations.

(define (adjust-unmutable-references! node)
  (if (and (call? node)
           (ref? (car (node-children node)))
           (eq? '#%unbox (var-id (ref-var (car (node-children node)))))
           (ref? (cadr (node-children node)))
           (not (mutable-var? (ref-var (cadr (node-children node))))))
      (let* ((parent (node-parent node)) (child (cadr (node-children node))))
        (set-node-parent! child parent)
        (when parent
          (set-node-children! parent
                              (map (lambda (c) (if (eq? c node) child c))
                                   (node-children parent))))
        child)
      (begin (for-each (lambda (n) (adjust-unmutable-references! n))
		       (node-children node))
             node)))
