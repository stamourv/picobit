#lang racket

(require "utilities.rkt" "env.rkt" "ast.rkt")

;-----------------------------------------------------------------------------

(provide immutable-var? mutable-var?
         toplevel-prc?
         toplevel-prc-with-non-rest-correct-calls?
         side-effect-less?)

(define (immutable-var? var) (null? (var-sets var)))
(define (mutable-var?   var) (not (immutable-var? var)))

(define (toplevel-prc? var)
  (and (not (mutable-var? var))
       (let ([d (var-defs var)])
         (and (pair? d)
              (null? (cdr d))
              (let ([val (child1 (car d))])
                (and (prc? val)
                     val))))))

(define (toplevel-prc-with-non-rest-correct-calls? var)
  (let ([prc (toplevel-prc? var)])
    (and prc
         (not (prc-rest? prc))
         (andmap (lambda (r)
                   (let ([parent (node-parent r)])
                     (and (call? parent)
                          (eq? (child1 parent) r)
                          (= (length (prc-params prc))
                             (- (length (node-children parent)) 1)))))
                 (var-refs var))
         prc)))

(define (side-effect-less? node) (or (cst? node) (ref? node) (prc? node)))

;-----------------------------------------------------------------------------

;; Free variable analysis.

(provide global-fv
         non-global-fv
         fv)

(require (except-in racket/set
                    set? set)) ; to avoid collision with the node type

;; varsets are eq? sets

(define (build-params-varset params)
  (list->seteq params))

;; These two are for outside consumption, so they return results as lists.
(define (global-fv node)
  (filter var-global?
          (set->list (fv node))))
(define (non-global-fv node)
  (filter (lambda (x) (not (var-global? x)))
          (set->list (fv node))))

(define (fv node)
  (match node
    [(? cst? node)
     (seteq)] ; empty varset
    [(ref _ '() var)
     (seteq var)] ; singleton varset
    [(def _ `(,val) var)
     (set-add (fv val) var)]
    [(set _ `(,val) var)
     (set-add (fv val) var)]
    [(prc _ `(,body) params rest? entry-label)
     (set-subtract
      (fv body)
      (build-params-varset params))]
    [(or (? if*? node) (? call? node) (? seq? node))
     (apply set-union (map fv (node-children node)))]
    [_
     (compiler-error "unknown expression type" node)]))
