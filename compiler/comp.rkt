#lang racket

(provide comp-none code->vector resolve-toplevel-labels!)
(require "utilities.rkt" "ir.rkt" "code-gen.rkt" "ast.rkt" "env.rkt"
         "analysis.rkt")


(define (comp-none node ctx)
  (match node
    [(or (? cst? node) (? ref? node) (? prc? node))
     ctx] ; we can drop any of these if we don't care about their value
    [(def _ `(,rhs) var)
     (if (toplevel-prc-with-non-rest-correct-calls? var)
         (comp-prc rhs #f ctx)
         (if (var-needed? var)
             (let ([ctx2 (comp-push rhs ctx)])
               (gen-set-global (var-id var) ctx2))
             (comp-none rhs ctx)))]
    [(set _ `(,rhs) var)
     (if (var-needed? var)
         (let ((ctx2 (comp-push rhs ctx)))
           (gen-set-global (var-id var) ctx2))
         (comp-none rhs ctx))]
    [(if* _ `(,tst ,thn ,els))
     (let*-values
         ([(ctx2 label-then)      (context-make-label ctx)]
          [(ctx3 label-else)      (context-make-label ctx2)]
          [(ctx4 label-then-join) (context-make-label ctx3)]
          [(ctx5 label-else-join) (context-make-label ctx4)]
          [(ctx6 label-join)      (context-make-label ctx5)]
          [(ctx7)  (comp-test tst label-then label-else ctx6)]
          [(ctx8)  (gen-goto
                    label-else-join
                    (comp-none els
                               (context-change-env2
                                (context-add-bb ctx7 label-else)
                                #f)))]
          [(ctx9)  (gen-goto
                    label-then-join
                    (comp-none thn
                               (context-change-env
                                (context-add-bb ctx8 label-then)
                                (context-env2 ctx7))))]
          [(ctx10) (gen-goto
                    label-join
                    (context-add-bb ctx9 label-else-join))]
          [(ctx11) (gen-goto
                    label-join
                    (context-add-bb ctx10 label-then-join))]
          [(ctx12) (context-add-bb ctx11 label-join)])
       ctx12)]
    
    [(? call? node)
     (comp-call node 'none ctx)]
    [(seq _ '())
     ctx]
    [(seq _ (? list? children))
     (let loop ([lst children]
                [ctx ctx])
       (if (null? (cdr lst))
           (comp-none (car lst) ctx)
           (loop (cdr lst)
                 (comp-none (car lst) ctx))))]
    [_
     (compiler-error "unknown expression type" node)]))

(define (comp-tail node ctx)
  (match node
    [(or (? cst? node) (? ref? node) (? def? node) (? set? node) (? prc? node))
     (gen-return (comp-push node ctx))]
    [(if* _ `(,tst ,thn ,els))
     (let*-values
         ([(ctx2 label-then) (context-make-label ctx)]
          [(ctx3 label-else) (context-make-label ctx2)]
          [(ctx4) (comp-test tst label-then label-else ctx3)]
          [(ctx5) (comp-tail els
                             (context-change-env2
                              (context-add-bb ctx4 label-else)
                              #f))]
          [(ctx6) (comp-tail thn
                             (context-change-env
                              (context-add-bb ctx5 label-then)
                              (context-env2 ctx4)))])
       ctx6)]
    [(? call? node)
     (comp-call node 'tail ctx)]
    [(seq _ '())
     (gen-return (gen-push-unspecified ctx))]
    [(seq _ (? list? children))
     (if (null? children)
         (gen-return (gen-push-unspecified ctx))
         (let loop ([lst children]
                    [ctx ctx])
           (if (null? (cdr lst))
               (comp-tail (car lst) ctx)
               (loop (cdr lst)
                     (comp-none (car lst) ctx)))))]
    [_
     (compiler-error "unknown expression type" node)]))

(define (comp-push node ctx)
  (match node
    [(cst _ '() val)
     (gen-push-constant val ctx)]
    [(ref _ '() var)
     (if (var-global? var)
         (if (null? (var-defs var))
             (compiler-error "undefined variable:" (var-id var))
             (let ([val (child1 (car (var-defs var)))])
               (if (and (not (mutable-var? var))
                        (cst? val)) ; immutable global, counted as cst
                   (gen-push-constant (cst-val val) ctx)
                   (gen-push-global   (var-id  var) ctx))))
         (gen-push-local-var (var-id var) ctx))]
    [(or (? def? node) (? set? node))
     (gen-push-unspecified (comp-none node ctx))]
    [(if* _ `(,tst ,thn ,els))
     (let*-values
         ([(ctx2 label-then)      (context-make-label ctx)]
          [(ctx3 label-else)      (context-make-label ctx2)]
          [(ctx4 label-then-join) (context-make-label ctx3)]
          [(ctx5 label-else-join) (context-make-label ctx4)]
          [(ctx6 label-join)      (context-make-label ctx5)]
          [(ctx7)  (comp-test tst label-then label-else ctx6)]
          [(ctx8)  (gen-goto
                    label-else-join
                    (comp-push els
                               (context-change-env2
                                (context-add-bb ctx7 label-else)
                                #f)))]
          [(ctx9)  (gen-goto
                    label-then-join
                    (comp-push thn
                               (context-change-env
                                (context-add-bb ctx8 label-then)
                                (context-env2 ctx7))))]
          [(ctx10) (gen-goto
                    label-join
                    (context-add-bb ctx9 label-else-join))]
          [(ctx11) (gen-goto
                    label-join
                    (context-add-bb ctx10 label-then-join))]
          [(ctx12) (context-add-bb ctx11 label-join)])
       ctx12)]
    [(? prc? node)
     (comp-prc node #t ctx)]
    [(? call? node)
     (comp-call node 'push ctx)]
    [(seq _ '())
     (gen-push-unspecified ctx)]
    [(seq _ (? list? children))
     (let loop ([lst children]
                [ctx ctx])
       (if (null? (cdr lst))
           (comp-push (car lst) ctx)
           (loop (cdr lst)
                 (comp-none (car lst) ctx))))]
    [_
     (compiler-error "unknown expression type" node)]))

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
  (let*-values
      ([(ctx2 label-entry)    (context-make-label ctx)]
       [(ctx3 label-continue) (context-make-label ctx2)]
       [(body-env) (prc->env node)]
       [(ctx4)
        (if closure?
            (build-closure label-entry (env-closed body-env) ctx3)
            ctx3)]
       [(ctx5) (gen-goto label-continue ctx4)]
       [(ctx6) (gen-entry (length (prc-params node))
                          (prc-rest? node)
                          (context-add-bb
                           (context-change-env ctx5
                                               body-env)
                           label-entry))]
       [(ctx7) (comp-tail (child1 node) ctx6)])
    (set-prc-entry-label! node label-entry)
    (context-add-bb (context-change-env ctx7 (context-env ctx5))
                    label-continue)))

(define (prc->env prc)
  (make-env
   (let ([params (prc-params prc)])
     (make-stack (length params) (map var-id params)))
   (map var-id (non-global-fv prc))))

(define (comp-call node reason orig-ctx)
  (match node
    [(call _ `(,op . ,args))
     (define nargs (length args))
     (define ctx
       (for/fold ([ctx orig-ctx])
           ([arg (in-list args)])
         ;; push all the arguments
         (comp-push arg ctx)))
     ;; generate the call itself
     (match op
       
       [(ref _ '() (? var-primitive var)) ; primitive call
        (define id         (var-id var))
        (define primitive  (var-primitive var))
        (define prim-nargs (primitive-nargs primitive))
        (define result-ctx
          (cond [(primitive-inliner primitive)
                 ((primitive-inliner primitive) ctx)]
                [(not (= nargs prim-nargs))
                 (compiler-error
                  "primitive called with wrong number of arguments"
                  id)]
                [else
                 (gen-prim id
                           prim-nargs
                           (primitive-unspecified-result? primitive)
                           ctx)]))
        (define unspecified? (primitive-unspecified-result? primitive))
        (define result
          (if unspecified?
              (gen-push-unspecified result-ctx)
              result-ctx))
        (case reason
          [(tail) (gen-return result)]
          [(push) result]
          [else   (if unspecified?
                      result-ctx
                      (gen-pop result-ctx))])]

       [(ref _ '() var)
        (=> unmatch)
        (cond [(toplevel-prc-with-non-rest-correct-calls? var)
               =>
               (lambda (prc)
                 (case reason
                   [(tail) (gen-jump-toplevel nargs prc ctx)]
                   [(push) (gen-call-toplevel nargs prc ctx)]
                   [else   (gen-pop (gen-call-toplevel nargs prc ctx))]))]
              [else (unmatch)])]
       
       [_
        (let ([ctx2 (comp-push op ctx)])
          (case reason
            [(tail) (gen-jump nargs ctx2)]
            [(push) (gen-call nargs ctx2)]
            [else   (gen-pop (gen-call nargs ctx2))]))])]))

(define (comp-test node label-true label-false ctx)
  (match node
    [(cst _ '() val)
     (let ([ctx2 (gen-goto (if val label-true label-false) ctx)])
       (context-change-env2 ctx2 (context-env ctx2)))]
    [(or (? ref? node) (? def? node) (? set? node) (? if*? node)
         (? call? node) (? seq? node))
     (let* ([ctx2 (comp-push node ctx)]
            [ctx3 (gen-goto-if-false label-false label-true ctx2)])
       (context-change-env2 ctx3 (context-env ctx3)))]
    [(? prc? node) ; always true
     (let ([ctx2 (gen-goto label-true ctx)])
       (context-change-env2 ctx2 (context-env ctx2)))]
    [_
     (compiler-error "unknown expression type" node)]))

;-----------------------------------------------------------------------------

(define (code->vector code)
  (let ((v (make-vector (+ (code-last-label code) 1))))
    (for-each
     (lambda (bb)
       (vector-set! v (bb-label bb) bb))
     (code-rev-bbs code))
    v))

(define (resolve-toplevel-labels! bbs)
  (for ([i (in-range (vector-length bbs))])
    (let* ([bb (vector-ref bbs i)]
           [rev-instrs (bb-rev-instrs bb)])
      (set-bb-rev-instrs!
       bb
       (map (match-lambda
              [`(,(and opcode (or 'call-toplevel 'jump-toplevel)) ,arg)
               `(,opcode ,(prc-entry-label arg))]
              [instr
               instr])
            rev-instrs)))))
