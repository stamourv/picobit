#lang racket

(provide (all-defined-out))
(require "node.rkt")
(require "env.rkt")

(define context-change-code
  (lambda (ctx code)
    (make-context code
                  (context-env ctx)
                  (context-env2 ctx))))

(define context-change-env
  (lambda (ctx env)
    (make-context (context-code ctx)
                  env
                  (context-env2 ctx))))

(define context-change-env2
  (lambda (ctx env2)
    (make-context (context-code ctx)
                  (context-env ctx)
                  env2)))

(define make-init-context
  (lambda ()
    (make-context (make-init-code)
                  (make-init-env)
                  #f)))

(define context-make-label
  (lambda (ctx)
    (context-change-code ctx (code-make-label (context-code ctx)))))

(define context-last-label
  (lambda (ctx)
    (code-last-label (context-code ctx))))

(define context-add-bb
  (lambda (ctx label)
    (context-change-code ctx (code-add-bb (context-code ctx) label))))

(define context-add-instr
  (lambda (ctx instr)
    (context-change-code ctx (code-add-instr (context-code ctx) instr))))

;; Representation of code.

(define-struct code (last-label rev-bbs))

(define-struct bb (label (rev-instrs #:mutable)))

(define make-init-code
  (lambda ()
    (make-code 0
               (list (make-bb 0 (list))))))

(define code-make-label
  (lambda (code)
    (let ((label (+ (code-last-label code) 1)))
      (make-code label
                 (code-rev-bbs code)))))

(define code-add-bb
  (lambda (code label)
    (make-code
     (code-last-label code)
     (cons (make-bb label '())
           (code-rev-bbs code)))))

(define code-add-instr
  (lambda (code instr)
    (let* ((rev-bbs (code-rev-bbs code))
           (bb (car rev-bbs))
           (rev-instrs (bb-rev-instrs bb)))
      (make-code
       (code-last-label code)
       (cons (make-bb (bb-label bb)
                      (cons instr rev-instrs))
             (cdr rev-bbs))))))

;; Representation of compile-time stack.

;;; A stack is a (make-stack size slots) where:
;;; - size is the number of slots
;;; - slots is a list of variables or #f in each slot
(define-struct stack (size slots))

(define make-init-stack
  (lambda ()
    (make-stack 0 '())))

(define stack-extend
  (lambda (x nb-slots stk)
    (let ((size (stack-size stk)))
      (make-stack
       (+ size nb-slots)
       (append (make-list nb-slots x) (stack-slots stk))))))

(define stack-discard
  (lambda (nb-slots stk)
    (let ((size (stack-size stk)))
      (make-stack
       (- size nb-slots)
       (list-tail (stack-slots stk) nb-slots)))))



;; Representation of compile-time environment.

(define-struct env (local closed))

(define make-init-env
  (lambda ()
    (make-env (make-init-stack)
              '())))

(define env-change-local
  (lambda (env local)
    (make-env local
              (env-closed env))))

(define env-change-closed
  (lambda (env closed)
    (make-env (env-local env)
              closed)))

(define pos-in-list
  (lambda (x lst)
    (let loop ((lst lst) (i 0))
      (cond ((not (pair? lst)) #f)
            ((eq? (car lst) x) i)
            (else (loop (cdr lst) (+ i 1)))))))

(define find-local-var
  (lambda (var env)
    (let ((i (pos-in-list var (stack-slots (env-local env)))))
      (or i
          (- (+ (pos-in-list var (env-closed env)) 1))))))


