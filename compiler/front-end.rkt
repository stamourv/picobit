#lang racket

(require "utilities.rkt" "ast.rkt" "env.rkt" "analysis.rkt" "primitives.rkt")

;; Front-end code transformations.

;------------------------------------------------------------------------------

(provide adjust-unmutable-references!)

(define (adjust-unmutable-references! node)
  (match node
    [(call parent `(,(ref _ '() (app var-id '#%unbox))
                    ,(and child (ref _ '() vv))))
     (=> unmatch)
     (cond [(immutable-var? vv)
            (set-node-parent! child parent)
            (when parent
              (set-node-children! parent
                                  (map (lambda (c) (if (eq? c node) child c))
                                       (node-children parent))))
            child]
           [else (unmatch)])]
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
    [(? cst? node)
     (void)]
    [(ref _ '() var)
     (mark-var! var)]
    [(def _ `(,val) var)
     (when (not (side-effect-less? val))
       (mark-needed-global-vars! val))]
    [(set _ `(,val) var)
     (mark-needed-global-vars! val)]
    [(if* _ `(,a ,b ,c))
     (mark-needed-global-vars! a)
     (mark-needed-global-vars! b)
     (mark-needed-global-vars! c)]
    [(prc _ `(,body) params rest? entry-label)
     (mark-needed-global-vars! body)]
    [(or (call _ children) (seq _ children))
     (for-each mark-needed-global-vars! (node-children node))]
    [_
     (compiler-error "unknown expression type" node)]))
