#lang racket

(provide (all-defined-out))
(require syntax/parse unstable/match racket/syntax)
(require "utilities.rkt" "env.rkt")

;; Syntax-tree node representation.

;; The AST is doubly linked, children point to their parent. This makes it
;; possible to crawl the tree backwards, which is useful for some analysis
;; and transformations.

;; In addition, variable objects are doubly linked as well. Defs, refs, sets
;; and prcs point to the variables involved, and the variables point back to
;; their definition, references and assignments. Again, this makes analysis
;; and transformations easier.

;; Important invariant: No node sharing.
;; If we create identical nodes, or copy an existing node, the results
;; must _not_ be eq?.
;; Otherwise, this may screw up accounting (e.g. for refs) or cycle
;; detection in analysis, etc.

(define-struct node (parent children) #:mutable #:transparent)

(define (child1 node) (car   (node-children node)))

(define (immutable-var? var) (null? (var-sets var)))
(define (mutable-var?   var) (not (immutable-var? var)))

;; If v is defined, return the node corresponding to its value.
;; Returns #f if something goes wrong.
(define (var-val v)
  (define def (var-def v))
  (and (immutable-var? v)
       def
       (not (prc? def)) ; var defined in a lambda, no fixed value
       (child1 def)))   ; rhs of a define


(define-struct (cst node) (val))
(define-struct (ref node) (var))
(define-struct (def node) (var)) ; children: (rhs)
(define-struct (set node) (var)) ; children: (rhs)
(define-struct (if* node) ())    ; children: (test then else)
(define-struct (prc node)        ; children: (body)
  ((params  #:mutable) ; listof var?
   rest?
   (entry-label #:mutable)))
(define-struct (call node) ())   ; children: (op . args)
(define-struct (seq  node) ())   ; children: (body ...)


;; parsing helpers
(define (extract-ids pattern)
  (syntax->list
   (syntax-parse pattern
     [(x:identifier ...)                #'(x ...)]
     [(x:identifier ... . y:identifier) #'(x ... y)])))
(define (has-rest-param? pattern)
  (syntax-parse pattern
    [(x:identifier ...)                #f]
    [(x:identifier ... . y:identifier) #t]))


;; AST construction helpers

(define (create-ref v)
  (define r (make-ref #f '() v)) ; parent needs to be set by caller
  (set-var-refs! v (cons r (var-refs v)))
  r)
;; Needs to be called every time we remove a ref from the AST to avoid
;; dangling references which hurt analysis precision.
(define (discard-ref r)
  (define var  (ref-var r))
  (define refs (var-refs var))
  (unless (memq r refs)
    (compiler-error "discard-ref: ref is not in the variable's refs"
                    (var-id var)))
  (set-var-refs! var (remq r refs)))
(define (discard-set s)
  (define var  (ref-var s))
  (define sets (var-sets var))
  (unless (memq s sets)
    (compiler-error "discard-set: set is not in the variable's sets"
                    (var-id var)))
  (set-var-sets! var (remq s sets)))

;; Crawls e and discards any refs, and sets it encounters, so that e can
;; be dropped without leaving dangling references.
(define (discard-node! e parent)
  (when (memq e (node-children (node-parent e)))
    (error "discard-node!: can't discard a node that's still attached"
           (node->expr e)))
  (set-node-parent! e #f)
  (when (ref? e) (discard-ref e))
  (when (set? e) (discard-set e))
  (define cs (node-children e))
  (set-node-children! e '()) ; detach children, so they can be discarded too
  (for ([c (in-list cs)]) (discard-node! c e)))

(define (create-prc children params rest?)
  (make-prc #f children params rest?
            #f)) ; entry-label, will be filled later


(define (fix-children-parent! p)
  (for-each (lambda (x) (set-node-parent! x p)) (node-children p)))

;; Note: if new is a descendant of old, it needs to be copied before begin
;; substituted in. This is because we discard old and all its descendants.
(define (substitute-child! parent old new)
  (define children (node-children parent))
  (unless (memq old children)
    (compiler-error "substitute-child!: old is not in children"
                    (node->expr old)))
  (set-node-parent! new parent)
  (set-node-children! parent (map (lambda (x) (if (eq? x old) new x))
                                  children))
  (discard-node! old parent))

;; Is c a descendant of p?
(define (inside? c p)
  (or (eq? c p)
      (and c ; not at the top of the program
           (inside? (node-parent c) p))))

;; Splice the child begin inside the parent begin in place of the old node.
(define (splice-begin! old child parent)
  (set-node-children!
   parent
   (apply append
          (for/list ([c (node-children parent)])
            (if (eq? c old) ; we replace that
                (node-children child)
                (list c))))) ; keep that one
  (fix-children-parent! parent))


;; Since nodes know their parents, we can't just reuse them directly.
;; For this reason, this is a deep copy.
;; Optionally takes a list of variable substitutions. When we copy lambdas,
;; we need to create new var objects, otherwise the copy's variables will be
;; the same as the original's.
(define (copy-node e [substs '()]) ; substs : (listof (pair var var))
  (define (maybe-substitute-var var)
    (cond [(assoc var substs var=?) => cdr]
          [else var]))
  (define new
    (match e
      ;; parent is left #f, caller must set it
      ;; children are copied below
      [(cst _ _ val) ; no need to copy val
       (make-cst #f '() val)]
      [(ref _ _ var) ; we may need to substitute
       (create-ref (maybe-substitute-var var))] ; registers the reference
      [(def _ _ var) ; only at the top-level, makes no sense to copy
       (compiler-error "copying the definition of" (var-id var))]
      [(set _ _ var) ; as above
       (make-set #f '() (maybe-substitute-var var))]
      [(if* _ _)
       (make-if* #f '())]
      [(prc _ _ params rest? entry)
       (define new (make-prc #f '() '() rest? entry))
       ;; we need to create new parameters, and replace the old ones in body
       ;; Note: with Racket identifiers being used for variables, we'll need
       ;; to freshen the new vars, otherwise the new ones will be
       ;; free-identifier=? with the old ones, and we don't want that!
       (define (copy-var v)
         (make-local-var (generate-temporary (var-id v)) new))
       (define new-params (map copy-var params))
       (set-prc-params! new new-params)
       new]
      [(call _ _)
       (make-call #f '())]
      [(seq _ _)
       (make-seq #f '())]))
  ;; If we're copying a lambda, we need to substitute the new one's params
  ;; for the original's
  (define new-substs
    (if (prc? new)
        (append (map cons (prc-params e) (prc-params new))
                substs)
        substs))
  (set-node-children! new (for/list ([c (in-list (node-children e))])
                            (copy-node c new-substs)))
  (fix-children-parent! new)
  new)


;; Pretty-printer, mostly for debugging

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
    [(prc _ `(,body) params rest? _)
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
