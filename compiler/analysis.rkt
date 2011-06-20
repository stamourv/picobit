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

(provide mark-needed-global-vars!)

(define (mark-needed-global-vars! global-env node)
  (define readyq
    (env-lookup global-env '#%readyq))

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
      (for-each
       (lambda (def)
         (let ((val (child1 def)))
           (when (side-effect-less? val)
             (mark! val))))
       (var-defs var))
      (when (eq? var readyq)
        (mark-var!
         (env-lookup global-env '#%start-first-process))
        (mark-var!
         (env-lookup global-env '#%exit)))))

  (define (side-effect-less? node)
    (or (cst? node)
        (ref? node)
        (prc? node)))

  (define (mark! node)
    (cond ((cst? node))
          ((ref? node)
           (let ((var (ref-var node)))
             (mark-var! var)))
          ((def? node)
           (let ((var (def-var node))
                 (val (child1 node)))
             (when (not (side-effect-less? val))
               (mark! val))))
          ((set? node)
           (let ((var (set-var node))
                 (val (child1 node)))
             (mark! val)))
          ((if*? node)
           (let ((a (list-ref (node-children node) 0))
                 (b (list-ref (node-children node) 1))
                 (c (list-ref (node-children node) 2)))
             (mark! a)
             (mark! b)
             (mark! c)))
          ((prc? node)
           (let ((body (list-ref (node-children node) 0)))
             (mark! body)))
          ((call? node)
           (for-each mark! (node-children node)))
          ((seq? node)
           (for-each mark! (node-children node)))
          (else
           (compiler-error "unknown expression type" node))))

  (mark! node))

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
  (cond ((cst? node)
         (seteq)) ; empty varset
        ((ref? node)
         (let ((var (ref-var node)))
           (seteq var))) ; singleton varset
        ((def? node)
         (let ((var (def-var node))
               (val (child1 node)))
           (set-add (fv val) var)))
        ((set? node)
         (let ((var (set-var node))
               (val (child1 node)))
           (set-add (fv val) var)))
        ((if*? node)
         (let ((a (list-ref (node-children node) 0))
               (b (list-ref (node-children node) 1))
               (c (list-ref (node-children node) 2)))
           (set-union (fv a) (fv b) (fv c))))
        ((prc? node)
         (let ((body (list-ref (node-children node) 0)))
           (set-subtract
            (fv body)
            (build-params-varset (prc-params node)))))
        ((call? node)
         (apply set-union (map fv (node-children node))))
        ((seq? node)
         (apply set-union (map fv (node-children node))))
        (else
         (compiler-error "unknown expression type" node))))
