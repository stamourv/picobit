#lang racket

(provide (all-defined-out))

(require srfi/4)

(define compiler-error
  (lambda (msg . others)
    (display "*** PICOBIT ERROR -- ")
    (display msg)
    (for-each (lambda (x) (display " ") (write x)) others)
    (newline)
    (exit 1)))

(define (self-eval? expr)
  (or (number?   expr)
      (char?     expr)
      (boolean?  expr)
      (string?   expr)
      (u8vector? expr)))


;; to control output level
(define show-size?           (make-parameter #f))
(define show-asm?            (make-parameter #f))
(define show-parsed?         (make-parameter #f))
(define show-post-front-end? (make-parameter #f))
(define stats?               (make-parameter #f))
