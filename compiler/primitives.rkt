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


;-----------------------------------------------------------------------------


;; name, n-args, encoding (opcode, prim no), maybe #:unspecified-result
;; Encodings must be kept in sync with the VM. It needs to agree with
;; the compiler on the instruction set.

(define-primitive #%number? 1 0)
(define-primitive #%+ 2 1)
(define-primitive #%- 2 2)
(define-primitive #%mul-non-neg 2 3)
(define-primitive #%div-non-neg 2 4)
(define-primitive #%rem-non-neg 2 5)
(define-primitive #%= 2 7)
(define-primitive #%< 2 8)
(define-primitive #%> 2 10)

(define-primitive #%pair? 1 12)
(define-primitive #%cons 2 13)
(define-primitive #%car 1 14)
(define-primitive #%cdr 1 15)
(define-primitive #%set-car! 2 16 #:unspecified-result)
(define-primitive #%set-cdr! 2 17 #:unspecified-result)
(define-primitive #%null? 1 18)

(define-primitive #%eq? 2 19)
(define-primitive #%not 1 20)

(define-primitive #%get-cont 0 21)
(define-primitive #%graft-to-cont 2 22)
(define-primitive #%return-to-cont 2 23)
(define-primitive #%halt 0 24 #:unspecified-result)

(define-primitive #%symbol? 1 25)
(define-primitive #%string? 1 26)
(define-primitive #%string->list 1 27)
(define-primitive #%list->string 1 28)

(define-primitive #%make-u8vector 1 29)
(define-primitive #%u8vector-ref 2 30)
(define-primitive #%u8vector-set! 3 31 #:unspecified-result)

(define-primitive #%print 1 32 #:unspecified-result)
(define-primitive #%clock 0 33)
(define-primitive #%motor 2 34 #:unspecified-result)
(define-primitive #%led 3 35 #:unspecified-result)
(define-primitive #%led2-color 1 36 #:unspecified-result)
(define-primitive #%getchar-wait 2 37)
(define-primitive #%putchar 2 38 #:unspecified-result)
(define-primitive #%beep 2 39 #:unspecified-result)
(define-primitive #%adc 1 40)

(define-primitive #%u8vector? 1 41)
(define-primitive #%sernum 0 42)
(define-primitive #%u8vector-length 1 43)
(define-primitive #%boolean? 1 48)
(define-primitive #%network-init 0 49 #:unspecified-result)
(define-primitive #%network-cleanup 0 50 #:unspecified-result)
(define-primitive #%receive-packet-to-u8vector 1 51)
(define-primitive #%send-packet-from-u8vector 2 52)
(define-primitive #%ior 2 53)
(define-primitive #%xor 2 54)
