#lang racket

(require "utilities.rkt" "ast.rkt" "env.rkt" "analysis.rkt" "primitives.rkt")

;; Front-end code transformations.

;------------------------------------------------------------------------------

(provide adjust-unmutable-references!)

(define (adjust-unmutable-references! node)
  (match node
    [(call parent `(,(ref _ '() (app var-id '#%unbox))
                    ,(and child (ref _ '() (? immutable-var? v)))))
     (set-node-parent! child parent)
     (when parent
       (set-node-children! parent (for/list ([c (node-children parent)])
                                    (if (eq? c node) child c))))
     child]
    [_
     (for-each adjust-unmutable-references! (node-children node))
     node]))

;-----------------------------------------------------------------------------

(provide mark-needed-global-vars!)

(define (mark-var! var)
  (when (and (var-global? var)
             (not (var-needed? var))
             ;; globals that obey the following conditions are considered
             ;; to be constants
             (not (and (not (mutable-var? var))
                       ;; to weed out primitives, which have no definitions
                       (> (length (var-defs var)) 0)
                       (cst? (child1 (car (var-defs var)))))))
    (set-var-needed?! var #t)
    (for ([def (in-list (var-defs var))])
      (let ([val (child1 def)])
        (when (side-effect-less? val)
          (mark-needed-global-vars! val))))))

(define (mark-needed-global-vars! node)
  (match node
    [(ref _ '() var)
     (mark-var! var)]
    [(def _ `(,val) var)
     (when (not (side-effect-less? val))
       (mark-needed-global-vars! val))]
    [(or (? cst? node) (? set? node) (? if*? node) (? prc? node)
         (? call? node) (? seq? node))
     (for-each mark-needed-global-vars! (node-children node))]
    [_
     (compiler-error "unknown expression type" node)]))
