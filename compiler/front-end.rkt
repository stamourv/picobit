#lang racket

(require "utilities.rkt" "ast.rkt" "env.rkt" "analysis.rkt")

;; Front-end code transformations.

;------------------------------------------------------------------------------

(provide adjust-unmutable-references!)

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

;-----------------------------------------------------------------------------

(provide mark-needed-global-vars!)

(define (mark-needed-global-vars! global-env prog)
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

  (for-each (lambda (node) (mark! node))
            prog))

;------------------------------------------------------------------------------

(provide extract-parts-top)

;; Last argument is always the parse function from parse.rkt, taken as argument
;; to avoid circular dependencies.
(define (extract-parts-top parsed-prog global-env parse)
  (extract-parts
   parsed-prog
   (lambda (defs after-defs)

     (define (make-seq-preparsed exprs)
       (let ((r (make-seq #f exprs)))
         (for-each (lambda (x) (set-node-parent! x r)) exprs)
         r))

     (define (make-call-preparsed exprs)
       (let ((r (make-call #f exprs)))
         (for-each (lambda (x) (set-node-parent! x r)) exprs)
         r))

     (if (var-needed?
          (env-lookup global-env '#%readyq))
         (make-seq-preparsed
          (list (make-seq-preparsed defs)
                (make-call-preparsed
                 (list (parse 'value '#%start-first-process global-env)
                       (let* ((pattern
                               '())
                              (ids
                               (extract-ids pattern))
                              (r
                               (make-prc #f
                                         '()
                                         #f
                                         (has-rest-param? pattern)
                                         #f))
                              (new-env
                               (env-extend global-env ids r))
                              (body
                               (make-seq-preparsed after-defs)))
                         (set-prc-params!
                          r
                          (map (lambda (id) (env-lookup new-env id))
                               ids))
                         (set-node-children! r (list body))
                         (set-node-parent! body r)
                         r)))
                (parse 'value
                       '(#%exit)
                       global-env)))
         (make-seq-preparsed
          (append defs
                  after-defs
                  (list (parse 'value
                               '(#%halt)
                               global-env))))))))

(define (extract-parts lst cont)
  (if (or (null? lst)
          (not (def? (car lst))))
      (cont '() lst)
      (extract-parts
       (cdr lst)
       (lambda (d ad)
         (cont (cons (car lst) d) ad)))))
