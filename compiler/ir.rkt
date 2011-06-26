#lang racket

(provide (all-defined-out))
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
  (let ((label (+ (code-last-label code) 1)))
    (make-code label
               (code-rev-bbs code))))

(define (code-add-bb code label)
  (make-code
   (code-last-label code)
   (cons (make-bb label '())
         (code-rev-bbs code))))

(define (code-add-instr code instr)
  (let* ((rev-bbs (code-rev-bbs code))
         (bb (car rev-bbs))
         (rev-instrs (bb-rev-instrs bb)))
    (make-code
     (code-last-label code)
     (cons (make-bb (bb-label bb)
                    (cons instr rev-instrs))
           (cdr rev-bbs)))))

;; Representation of compile-time stack.

;;; A stack is a (make-stack size slots) where:
;;; - size is the number of slots
;;; - slots is a list of variables or #f in each slot
(define-struct stack (size slots) #:transparent)

(define (make-init-stack)
  (make-stack 0 '()))

(define (stack-extend x nb-slots stk)
  (let ((size (stack-size stk)))
    (make-stack
     (+ size nb-slots)
     (append (make-list nb-slots x) (stack-slots stk)))))

(define (stack-discard nb-slots stk)
  (let ((size (stack-size stk)))
    (make-stack
     (- size nb-slots)
     (list-tail (stack-slots stk) nb-slots))))



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

(define (pos-in-list x lst)
  (let loop ((lst lst) (i 0))
    (cond ((not (pair? lst)) #f)
          ((eq? (car lst) x) i)
          (else (loop (cdr lst) (+ i 1))))))

(define (find-local-var var env)
  (let ((i (pos-in-list var (stack-slots (env-local env)))))
    (or i
        (- (+ (pos-in-list var (env-closed env)) 1)))))
