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

(define readyq (env-lookup global-env '#%readyq))

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
          (mark! val))))
    (when (eq? var readyq)
      (mark-var! (env-lookup global-env '#%start-first-process))
      (mark-var! (env-lookup global-env '#%exit)))))

(define (mark! node)
  (match node
    [(? cst? node)
     (void)]
    [(ref _ '() var)
     (mark-var! var)]
    [(def _ `(,val) var)
     (when (not (side-effect-less? val))
       (mark! val))]
    [(set _ `(,val) var)
     (mark! val)]
    [(if* _ `(,a ,b ,c))
     (mark! a)
     (mark! b)
     (mark! c)]
    [(prc _ `(,body) params rest? entry-label)
     (mark! body)]
    [(or (call _ children) (seq _ children))
     (for-each mark! (node-children node))]
    [_
     (compiler-error "unknown expression type" node)]))

(define (mark-needed-global-vars! prog)
  (for-each mark! prog))

;------------------------------------------------------------------------------

(provide extract-parts-top)

(define (make-preparsed maker exprs)
  (let ([r (maker #f exprs)])
    (for ([x (in-list exprs)]) (set-node-parent! x r))
    r))

;; Last argument is always the parse function from parse.rkt, taken as
;; argument to avoid circular dependencies.
(define (extract-parts-top parsed-prog global-env parse)
  (define-values (defs after-defs)
    (partition def? parsed-prog))
  (if (var-needed? (env-lookup global-env '#%readyq))
      (make-preparsed
       make-seq
       (list (make-preparsed make-seq defs)
             (make-preparsed
              make-call
              (list (parse 'value '#%start-first-process global-env)
                    (let* ([pattern '()]
                           [ids     (extract-ids pattern)]
                           [r       (make-prc #f '() #f
                                              (has-rest-param? pattern)
                                              #f)]
                           [new-env (env-extend global-env ids r)]
                           [body    (make-preparsed make-seq after-defs)])
                      (set-prc-params!
                       r
                       (map (lambda (id) (env-lookup new-env id))
                            ids))
                      (set-node-children! r (list body))
                      (set-node-parent! body r)
                      r)))
             (parse 'value '(#%exit) global-env)))
      (make-preparsed
       make-seq
       (append defs
               after-defs
               (list (parse 'value '(#%halt) global-env))))))
