#lang racket

(provide (all-defined-out))
(require srfi/1)
(require "ast.rkt" "env.rkt")

(define-struct context (code env env2) #:transparent)

(define (context-change-code ctx code)
  (make-context code
                (context-env ctx)
                (context-env2 ctx)))

(define (context-change-env ctx env)
  (make-context (context-code ctx)
                env
                (context-env2 ctx)))

(define (context-change-env2 ctx env2)
  (make-context (context-code ctx)
                (context-env ctx)
                env2))

(define (make-init-context)
  (make-context (make-init-code)
                (make-init-env)
                #f))

(define (context-make-label ctx)
  (let ([r (context-change-code ctx (code-make-label (context-code ctx)))])
    (values r (code-last-label (context-code r)))))

(define (context-add-bb ctx label)
  (context-change-code ctx (code-add-bb (context-code ctx) label)))

(define (context-add-instr ctx instr)
  (context-change-code ctx (code-add-instr (context-code ctx) instr)))

;; Representation of code.

(define-struct code (last-label rev-bbs) #:transparent)

(define-struct bb (label (rev-instrs #:mutable)) #:transparent)

(define (make-init-code)
  (make-code 0
             (list (make-bb 0 (list)))))

(define (code-make-label code)
  (make-code (+ (code-last-label code) 1)
             (code-rev-bbs code)))

(define (code-add-bb code label)
  (make-code (code-last-label code)
             (cons (make-bb label '())
                   (code-rev-bbs code))))

(define (code-add-instr cur-code instr)
  (match cur-code
    [(code last-label `(,(bb label rev-instrs) . ,rest))
     (make-code last-label
                (cons (make-bb label
                               (cons instr rev-instrs))
                      rest))]))


;; Representation of compile-time stack.

;;; A stack is a (make-stack size slots) where:
;;; - size is the number of slots
;;; - slots is a list of variables or #f in each slot
(define-struct stack (size slots) #:transparent)

(define (make-init-stack)
  (make-stack 0 '()))

(define (stack-extend x nb-slots stk)
  (match stk
    [(stack size slots)
     (make-stack (+ size nb-slots)
                 (append (make-list nb-slots x) slots))]))

(define (stack-discard nb-slots stk)
  (match stk
    [(stack size slots)
     (make-stack
      (- size nb-slots)
      (list-tail slots nb-slots))]))



;; Representation of compile-time environment.

(define-struct env (local closed) #:transparent)

(define (make-init-env)
  (make-env (make-init-stack)
            '()))

(define (env-change-local env local)
  (make-env local
            (env-closed env)))

(define (env-change-closed env closed)
  (make-env (env-local env)
            closed))

(define (find-local-var var env)
  (define target? (lambda (x) (eq? x var)))
  (or (list-index target? (stack-slots (env-local env)))
      (- (+ (list-index target? (env-closed env)) 1))))
