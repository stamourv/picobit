#lang racket

(require "utilities.rkt" "env.rkt" "ast.rkt" "primitives.rkt")

;-----------------------------------------------------------------------------

(provide immutable-var? mutable-var?
         toplevel-prc?
         toplevel-prc-with-non-rest-correct-calls?
         side-effect-less? side-effect-oblivious?)

(define (immutable-var? var) (null? (var-sets var)))
(define (mutable-var?   var) (not (immutable-var? var)))

(define (toplevel-prc? var)
  (and (not (mutable-var? var))
       (let ([val (var-val var)])
         (and (prc? val)
              val))))

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
                 ;; may point to refs that are not in the program anymore
                 ;; this makes the analysis conservative, so we're safe
                 (var-refs var))
         prc)))

;; oblivious? is true if we want to check for side-effect obliviousless, which
;; is stronger
(define (side-effect-less? node [oblivious? #f] [seen '()])
  (and (or (cst? node) (prc? node) ; values
      ;; mutable var references are side-effect-less, but not oblivious
      (and (ref? node)
           (not (and oblivious? (mutable-var? (ref-var node)))))
      (and (or (seq? node) (if*? node))
           (for/and ([c (in-list (node-children node))])
             (side-effect-less? c oblivious? seen)))
      (and (call? node)
           (for/and ([c (in-list (cdr (node-children node)))]) ; args
             (side-effect-less? c oblivious? seen))
           (let ([op (car (node-children node))])
             (cond [(prc? op)
                    (side-effect-less? oblivious? (child1 op))] ; body
                   [(ref? op)
                    (or (let* ([var  (ref-var op)]
                               [prim (var-primitive var)])
                          ;; has a folder implies side-effect-less?
                          (and prim (primitive-constant-folder prim)
                               ;; for obliviousness, we also need it not to
                               ;; access mutable state
                               (if oblivious?
                                   (not (memf (lambda (x) (var=? x var))
                                              mutable-data-accessors))
                                   #t)))
                        (let* ([var (ref-var op)]
                               [val (var-val var)])
                          ;; refers to a side-effect-less? proc
                          ;; to avoid non-termination, we reject recursive funs
                          ;; Note: we could chase references further.
                          ;; we currently refet a ref to a ref of a
                          ;; side-effect-less? proc
                          (and (prc? val)
                               (not (for/or ([s (in-list seen)]) (var=? s var)))
                               (side-effect-less? (child1 val) ; body
                                                  oblivious?
                                                  (cons var seen)))))]))))))
;; could look into if*, seq, etc in operator position, making sure it refers to
;; a side-effect-less? proc (refs encountered during that are not automatically
;; ok)

;; The result of this expression does not depend on other side effects.
;; Implies: side-effect-less?
;; Corollary: this expression can be moved.
(define (side-effect-oblivious? node)
  (side-effect-less? node #t))

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
    [(prc _ `(,body) params _ _)
     (set-subtract
      (fv body)
      (build-params-varset params))]
    [(or (? if*? node) (? call? node) (? seq? node))
     (apply set-union (map fv (node-children node)))]
    [_
     (compiler-error "unknown expression type" node)]))
