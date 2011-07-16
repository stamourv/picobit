#lang racket

(require racket/mpair)
(require srfi/4)
(require "env.rkt")

;-----------------------------------------------------------------------------

(provide global-env)
(define global-env (mlist))

(provide primitive-encodings)
(define primitive-encodings '())


(define-syntax define-primitive
  (syntax-rules ()
    [(define-primitive name nargs encoding)
     (define-primitive name nargs encoding #:uns-res? #f #:folder #f)]
    ;; can't have both unspecified results and a constant folder
    [(define-primitive name nargs encoding #:unspecified-result)
     (define-primitive name nargs encoding #:uns-res? #t #:folder #f)]
    [(define-primitive name nargs encoding #:constant-folder folder)
     (define-primitive name nargs encoding #:uns-res? #f #:folder folder)]
    [(define-primitive name nargs encoding #:uns-res? uns? #:folder folder)
     (let ([prim (make-var 'name #t '() '() '() #f
                           (make-primitive nargs folder uns?))])
       (set! global-env (mcons prim global-env))
       (set! primitive-encodings
             (dict-set primitive-encodings 'name encoding)))]))

(include "gen.primitives.rkt")
