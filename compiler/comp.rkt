#lang racket

(provide comp-none code->vector resolve-toplevel-labels!)
(require "utilities.rkt" "context.rkt" "code-gen.rkt" "ast.rkt" "env.rkt"
         "analysis.rkt")


(define (comp-none node ctx)
  (cond ((or (cst? node)
             (ref? node)
             (prc? node))
         ctx)
        
        ((def? node)
         (let ((var (def-var node)))
           (if (toplevel-prc-with-non-rest-correct-calls? var)
               (comp-prc (child1 node) #f ctx)
               (if (var-needed? var)
                   (let ((ctx2 (comp-push (child1 node) ctx)))
                     (gen-set-global (var-id var) ctx2))
                   (comp-none (child1 node) ctx)))))
        
        ((set? node)
         (let ((var (set-var node)))
           (if (var-needed? var)
               (let ((ctx2 (comp-push (child1 node) ctx)))
                 (gen-set-global (var-id var) ctx2))
               (comp-none (child1 node) ctx))))
        
        ((if*? node)
         (let* ((ctx2
                 (context-make-label ctx))
                (label-then
                 (context-last-label ctx2))
                (ctx3
                 (context-make-label ctx2))
                (label-else
                 (context-last-label ctx3))
                (ctx4
                 (context-make-label ctx3))
                (label-then-join
                 (context-last-label ctx4))
                (ctx5
                 (context-make-label ctx4))
                (label-else-join
                 (context-last-label ctx5))
                (ctx6
                 (context-make-label ctx5))
                (label-join
                 (context-last-label ctx6))
                (ctx7
                 (comp-test (child1 node) label-then label-else ctx6))
                (ctx8
                 (gen-goto
                  label-else-join
                  (comp-none (child3 node)
                             (context-change-env2
                              (context-add-bb ctx7 label-else)
                              #f))))
                (ctx9
                 (gen-goto
                  label-then-join
                  (comp-none (child2 node)
                             (context-change-env
                              (context-add-bb ctx8 label-then)
                              (context-env2 ctx7)))))
                (ctx10
                 (gen-goto
                  label-join
                  (context-add-bb ctx9 label-else-join)))
                (ctx11
                 (gen-goto
                  label-join
                  (context-add-bb ctx10 label-then-join)))
                (ctx12
                 (context-add-bb ctx11 label-join)))
           ctx12))

        ((call? node)
         (comp-call node 'none ctx))

        ((seq? node)
         (let ((children (node-children node)))
           (if (null? children)
               ctx
               (let loop ((lst children)
                          (ctx ctx))
                 (if (null? (cdr lst))
                     (comp-none (car lst) ctx)
                     (loop (cdr lst)
                           (comp-none (car lst) ctx)))))))

        (else
         (compiler-error "unknown expression type" node))))

(define (comp-tail node ctx)
  (cond ((or (cst? node)
             (ref? node)
             (def? node)
             (set? node)
             (prc? node))
         (gen-return (comp-push node ctx)))

        ((if*? node)
         (let* ((ctx2
                 (context-make-label ctx))
                (label-then
                 (context-last-label ctx2))
                (ctx3
                 (context-make-label ctx2))
                (label-else
                 (context-last-label ctx3))
                (ctx4
                 (comp-test (child1 node) label-then label-else ctx3))
                (ctx5
                 (comp-tail (child3 node)
                            (context-change-env2
                             (context-add-bb ctx4 label-else)
                             #f)))
                (ctx6
                 (comp-tail (child2 node)
                            (context-change-env
                             (context-add-bb ctx5 label-then)
                             (context-env2 ctx4)))))
           ctx6))
        
        ((call? node)
         (comp-call node 'tail ctx))
        
        ((seq? node)
         (let ((children (node-children node)))
           (if (null? children)
               (gen-return (gen-push-unspecified ctx))
               (let loop ((lst children)
                          (ctx ctx))
                 (if (null? (cdr lst))
                     (comp-tail (car lst) ctx)
                     (loop (cdr lst)
                           (comp-none (car lst) ctx)))))))
        
        (else
         (compiler-error "unknown expression type" node))))

(define (comp-push node ctx)
  (cond ((cst? node)
         (let ((val (cst-val node)))
           (gen-push-constant val ctx)))

        ((ref? node)
         (let ((var (ref-var node)))
           (if (var-global? var)
               (if (null? (var-defs var))
                   (compiler-error "undefined variable:" (var-id var))
                   (let ((val (child1 (car (var-defs var)))))
                     (if (and (not (mutable-var? var))
                              (cst? val)) ;; immutable global, counted as cst
                         (gen-push-constant (cst-val val) ctx)
                         (gen-push-global (var-id var) ctx))))
               (gen-push-local-var (var-id var) ctx))))

        ((or (def? node)
             (set? node))
         (gen-push-unspecified (comp-none node ctx)))

        ((if*? node)
         (let* ((ctx2
                 (context-make-label ctx))
                (label-then
                 (context-last-label ctx2))
                (ctx3
                 (context-make-label ctx2))
                (label-else
                 (context-last-label ctx3))
                (ctx4
                 (context-make-label ctx3))
                (label-then-join
                 (context-last-label ctx4))
                (ctx5
                 (context-make-label ctx4))
                (label-else-join
                 (context-last-label ctx5))
                (ctx6
                 (context-make-label ctx5))
                (label-join
                 (context-last-label ctx6))
                (ctx7
                 (comp-test (child1 node) label-then label-else ctx6))
                (ctx8
                 (gen-goto
                  label-else-join
                  (comp-push (child3 node)
                             (context-change-env2
                              (context-add-bb ctx7 label-else)
                              #f))))
                (ctx9
                 (gen-goto
                  label-then-join
                  (comp-push (child2 node)
                             (context-change-env
                              (context-add-bb ctx8 label-then)
                              (context-env2 ctx7)))))
                (ctx10
                 (gen-goto
                  label-join
                  (context-add-bb ctx9 label-else-join)))
                (ctx11
                 (gen-goto
                  label-join
                  (context-add-bb ctx10 label-then-join)))
                (ctx12
                 (context-add-bb ctx11 label-join)))
           ctx12))

        ((prc? node)
         (comp-prc node #t ctx))

        ((call? node)
         (comp-call node 'push ctx))

        ((seq? node)
         (let ((children (node-children node)))
           (if (null? children)
               (gen-push-unspecified ctx)
               (let loop ((lst children)
                          (ctx ctx))
                 (if (null? (cdr lst))
                     (comp-push (car lst) ctx)
                     (loop (cdr lst)
                           (comp-none (car lst) ctx)))))))

        (else
         (compiler-error "unknown expression type" node))))

(define (build-closure label-entry vars ctx)

  (define (build vars ctx)
    (if (null? vars)
        (gen-push-constant '() ctx)
        (gen-prim '#%cons
                  2
                  #f
                  (build (cdr vars)
                         (gen-push-local-var (car vars) ctx)))))

  (if (null? vars)
      (gen-closure label-entry
                   (gen-push-constant '() ctx))
      (gen-closure label-entry
                   (build vars ctx))))

(define (comp-prc node closure? ctx)
  (let* ((ctx2
          (context-make-label ctx))
         (label-entry
          (context-last-label ctx2))
         (ctx3
          (context-make-label ctx2))
         (label-continue
          (context-last-label ctx3))
         (body-env
          (prc->env node))
         (ctx4
          (if closure?
              (build-closure label-entry (env-closed body-env) ctx3)
              ctx3))
         (ctx5
          (gen-goto label-continue ctx4))
         (ctx6
          (gen-entry (length (prc-params node))
                     (prc-rest? node)
                     (context-add-bb (context-change-env ctx5
                                                         body-env)
                                     label-entry)))
         (ctx7
          (comp-tail (child1 node) ctx6)))
    (set-prc-entry-label! node label-entry)
    (context-add-bb (context-change-env ctx7 (context-env ctx5))
                    label-continue)))

(define (prc->env prc)
  (make-env
   (let ((params (prc-params prc)))
     (make-stack (length params) (map var-id params)))
   (map var-id (non-global-fv prc))))

(define (comp-call node reason ctx)
  (let* ((op (child1 node))
         (args (cdr (node-children node)))
         (nargs (length args)))
    (let loop ((lst args)
               (ctx ctx))
      (if (pair? lst)

          ;; push all the arguments
          (let ((arg (car lst)))
            (loop (cdr lst)
                  (comp-push arg ctx)))

          ;; generate the call itself
          (cond [(and (ref? op)
                      (var-primitive (ref-var op)))
                 ;; primitive call
                 (let* ((var (ref-var op))
                        (id (var-id var))
                        (primitive (var-primitive var))
                        (prim-nargs (primitive-nargs primitive)))

                   (define use-result
                     (lambda (ctx2)
                       (cond ((eq? reason 'tail)
                              (gen-return
                               (if (primitive-unspecified-result? primitive)
                                   (gen-push-unspecified ctx2)
                                   ctx2)))
                             ((eq? reason 'push)
                              (if (primitive-unspecified-result? primitive)
                                  (gen-push-unspecified ctx2)
                                  ctx2))
                             (else
                              (if (primitive-unspecified-result? primitive)
                                  ctx2
                                  (gen-pop ctx2))))))

                   (use-result
                    (if (primitive-inliner primitive)
                        ((primitive-inliner primitive) ctx)
                        (if
                         (not (= nargs prim-nargs))
                         (compiler-error
                          "primitive called with wrong number of arguments"
                          id)
                         (gen-prim
                          id
                          prim-nargs
                          (primitive-unspecified-result? primitive)
                          ctx)))))]
                
                
                ((and (ref? op)
                      (toplevel-prc-with-non-rest-correct-calls?
                       (ref-var op)))
                 =>
                 (lambda (prc)
                   (cond ((eq? reason 'tail)
                          (gen-jump-toplevel nargs prc ctx))
                         ((eq? reason 'push)
                          (gen-call-toplevel nargs prc ctx))
                         (else
                          (gen-pop (gen-call-toplevel nargs prc ctx))))))

                (else
                 (let ((ctx2 (comp-push op ctx)))
                   (cond ((eq? reason 'tail)
                          (gen-jump nargs ctx2))
                         ((eq? reason 'push)
                          (gen-call nargs ctx2))
                         (else
                          (gen-pop (gen-call nargs ctx2)))))))))))

(define (comp-test node label-true label-false ctx)
  (cond ((cst? node)
         (let ((ctx2
                (gen-goto
                 (let ((val (cst-val node)))
                   (if val
                       label-true
                       label-false))
                 ctx)))
           (context-change-env2 ctx2 (context-env ctx2))))

        ((or (ref? node)
             (def? node)
             (set? node)
             (if*? node)
             (call? node)
             (seq? node))
         (let* ((ctx2
                 (comp-push node ctx))
                (ctx3
                 (gen-goto-if-false label-false label-true ctx2)))
           (context-change-env2 ctx3 (context-env ctx3))))

        ((prc? node)
         (let ((ctx2
                (gen-goto label-true ctx)))
           (context-change-env2 ctx2 (context-env ctx2))))

        (else
         (compiler-error "unknown expression type" node))))

;-----------------------------------------------------------------------------

(define (code->vector code)
  (let ((v (make-vector (+ (code-last-label code) 1))))
    (for-each
     (lambda (bb)
       (vector-set! v (bb-label bb) bb))
     (code-rev-bbs code))
    v))

(define (resolve-toplevel-labels! bbs)
  (let loop ((i 0))
    (when (< i (vector-length bbs))
      (let* ((bb (vector-ref bbs i))
             (rev-instrs (bb-rev-instrs bb)))
        (set-bb-rev-instrs!
         bb
         (map (lambda (instr)
                (let ((opcode (car instr)))
                  (cond ((eq? opcode 'call-toplevel)
                         (list opcode
                               (prc-entry-label (cadr instr))))
                        ((eq? opcode 'jump-toplevel)
                         (list opcode
                               (prc-entry-label (cadr instr))))
                        (else
                         instr))))
              rev-instrs))
        (loop (+ i 1))))))
