#lang racket

(require "utilities.rkt" "env.rkt" "ast.rkt")

;-----------------------------------------------------------------------------

(provide mutable-var?
         toplevel-prc?
         toplevel-prc-with-non-rest-correct-calls?)

(define (mutable-var? var) (not (null? (var-sets var))))

(define (toplevel-prc? var)
  (and (not (mutable-var? var))
       (let ((d (var-defs var)))
         (and (pair? d)
              (null? (cdr d))
              (let ((val (child1 (car d))))
                (and (prc? val)
                     val))))))

(define (toplevel-prc-with-non-rest-correct-calls? var)
  (let ((prc (toplevel-prc? var)))
    (and prc
         (not (prc-rest? prc))
         (andmap (lambda (r)
                   (let ((parent (node-parent r)))
                     (and (call? parent)
                          (eq? (child1 parent) r)
                          (= (length (prc-params prc))
                             (- (length (node-children parent)) 1)))))
                 (var-refs var))
         prc)))

;-----------------------------------------------------------------------------

;; Free variable analysis.

(provide global-fv
         non-global-fv
         fv)

(require (except-in racket/set
                    set? set)) ; to avoid collision with the node type
(define (varset-empty)                  (seteq))
(define (varset-singleton x)            (seteq x))
(define (list->varset lst)              (list->seteq lst))
(define (varset->list set)              (set->list set))
(define (varset-size set)               (set-count set))
(define (varset-empty? set)             (set-empty? set))
(define (varset-member? x set)          (set-member? set x))
(define (varset-adjoin set x)           (set-add set x))
(define (varset-remove set x)           (set-remove set x))
(define (varset-equal? s1 s2)           (equal? s1 s2))
(define (varset-subset? s1 s2)          (subset? s1 s2))
(define (varset-difference set1 set2)   (set-subtract set1 set2))
(define (varset-union set1 set2)        (set-union set1 set2))
(define (varset-intersection set1 set2) (set-intersect set1 set2))
(define (varset-union-multi sets)       (apply set-union sets))

(define (build-params-varset params)
  (list->varset params))

;; These two are for outside consumption, so they return results as lists.
(define (global-fv node)
  (filter var-global?
          (varset->list (fv node))))
(define (non-global-fv node)
  (filter (lambda (x) (not (var-global? x)))
          (varset->list (fv node))))

(define (fv node)
  (cond ((cst? node)
         (varset-empty))
        ((ref? node)
         (let ((var (ref-var node)))
           (varset-singleton var)))
        ((def? node)
         (let ((var (def-var node))
               (val (child1 node)))
           (varset-union
            (varset-singleton var)
            (fv val))))
        ((set? node)
         (let ((var (set-var node))
               (val (child1 node)))
           (varset-union
            (varset-singleton var)
            (fv val))))
        ((if*? node)
         (let ((a (list-ref (node-children node) 0))
               (b (list-ref (node-children node) 1))
               (c (list-ref (node-children node) 2)))
           (varset-union-multi (list (fv a) (fv b) (fv c)))))
        ((prc? node)
         (let ((body (list-ref (node-children node) 0)))
           (varset-difference
            (fv body)
            (build-params-varset (prc-params node)))))
        ((call? node)
         (varset-union-multi (map fv (node-children node))))
        ((seq? node)
         (varset-union-multi (map fv (node-children node))))
        (else
         (compiler-error "unknown expression type" node))))
