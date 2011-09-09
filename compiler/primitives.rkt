#lang racket

(require racket/mpair)
(require "env.rkt")

;-----------------------------------------------------------------------------

(provide global-env)
(define global-env (mlist))

(provide primitive-encodings)
(define primitive-encodings '())


(define-syntax define-primitive
  (syntax-rules ()
    [(define-primitive name nargs encoding)
     (define-primitive name nargs encoding #:uns-res? #f)]
    [(define-primitive name nargs encoding #:unspecified-result)
     (define-primitive name nargs encoding #:uns-res? #t)]
    [(define-primitive name nargs encoding #:uns-res? uns?)
     (let ([prim (make-var 'name #t '() '() '() #f
                           (make-primitive nargs #f uns?))])
       (set! global-env (mcons prim global-env))
       (set! primitive-encodings
             (dict-set primitive-encodings 'name encoding)))]))

(include "gen.primitives.rkt")
