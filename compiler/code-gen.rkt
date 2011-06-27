#lang racket

(provide (all-defined-out))
(require "ir.rkt")

;; Code generation utilities.
;; Each of these adds an IR instruction to the code stream.

(define (gen-instruction instr nb-pop nb-push ctx)
  (let* ([env (context-env ctx)]
         [stk (stack-extend #f
                            nb-push
                            (stack-discard nb-pop (env-local env)))])
    (context-add-instr (context-change-env ctx (env-change-local env stk))
                       instr)))

(define (gen-entry nparams rest? ctx)
  (gen-instruction `(entry ,nparams ,rest?) 0 0 ctx))

(define (gen-push-constant val ctx)
  (gen-instruction `(push-constant ,val) 0 1 ctx))

(define (gen-push-unspecified ctx)
  (gen-push-constant #f ctx))

(define (gen-push-local-var var ctx)
  (let ([i (find-local-var var (context-env ctx))])
    (if (>= i 0)
        (gen-push-stack i ctx)
        (gen-push-stack ; in the closed env, past the local variables
         (+ (- -1 i)
            (length (stack-slots (env-local (context-env ctx))))) ctx))))

(define (gen-push-stack pos ctx)
  (gen-instruction `(push-stack ,pos) 0 1 ctx))

(define (gen-push-global var ctx)
  (gen-instruction `(push-global ,var) 0 1 ctx))

(define (gen-set-global var ctx)
  (gen-instruction `(set-global ,var) 1 0 ctx))

(define (gen-call nargs ctx)
  (gen-instruction `(call ,nargs) (+ nargs 1) 1 ctx))

(define (gen-jump nargs ctx)
  (gen-instruction `(jump ,nargs) (+ nargs 1) 1 ctx))

(define (gen-call-toplevel nargs id ctx)
  (gen-instruction `(call-toplevel ,id) nargs 1 ctx))

(define (gen-jump-toplevel nargs id ctx)
  (gen-instruction `(jump-toplevel ,id) nargs 1 ctx))

(define (gen-goto label ctx)
  (gen-instruction `(goto ,label) 0 0 ctx))

(define (gen-goto-if-false label-false label-true ctx)
  (gen-instruction `(goto-if-false ,label-false ,label-true) 1 0 ctx))

(define (gen-closure label-entry ctx)
  (gen-instruction `(closure ,label-entry) 1 1 ctx))

(define (gen-prim id nargs unspec-result? ctx)
  (gen-instruction `(prim ,id)
                   nargs
                   (if unspec-result? 0 1)
                   ctx))

(define (gen-pop ctx)
  (gen-instruction '(pop) 1 0 ctx))

(define (gen-return ctx)
  (let ([ss (stack-size (env-local (context-env ctx)))])
    (gen-instruction '(return) ss 0 ctx)))
