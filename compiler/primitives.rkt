#lang racket

(require racket/mpair unstable/sequence)
(require srfi/4)
(require "env.rkt"
         "parser.rkt" "front-end.rkt") ; to build the eta-expansions

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
     (let ([prim (make-var
                  'name #t '() '() '() #f
                  (make-primitive
                   nargs #f
                   ;; eta-expansion, for higher-order uses, created lazily, to
                   ;; avoid parsing before all the primitives are defined
                   (lambda ()
                     (define res #f)
                     (or res
                         (begin (set! res (create-eta-expansion 'name nargs))
                                res)))
              uns?))])
       (set! global-env (mcons prim global-env))
       (set! primitive-encodings
             (dict-set primitive-encodings 'name encoding)))]))

(define (create-eta-expansion name nargs)
  (define params (build-list nargs (lambda (x) (gensym))))
  (define node
    (parse #f `(lambda ,params (,name ,@params)) global-env))
  (adjust-unmutable-references! node)
  node)

(include "gen.primitives.rkt")


;; Since constant folding is a compiler-only concept, it doesn't make
;; much sense to add folders to primitives in the VM, where primitives
;; are defined.
;; Instead, we add the constant folders post-facto. This requires that
;; the foldable primitives actually be defined, though. Since folding
;; is used for "essential" primitives, that shouldn't be an issue.

(define (add-constant-folder name folder)
  (define prim (var-primitive (env-lookup global-env name)))
  (set-primitive-constant-folder! prim folder))

(define folders
  `((number?           . ,number?)
    (#%+               . ,+)
    (#%-               . ,-)
    (#%mul-non-neg     . ,*)
    (#%div-non-neg     . ,quotient)
    (#%rem-non-neg     . ,remainder)
    (=                 . ,=)
    (<                 . ,<)
    (>                 . ,>)
    (pair?             . ,pair?)
    (car               . ,car)
    (cdr               . ,cdr)
    (null?             . ,null?)
    (eq?               . ,eq?)
    (not               . ,not)
    (symbol?           . ,symbol?)
    (string?           . ,string?)
    (string->list      . ,string->list)
    (list->string      . ,list->string)
    (u8vector-ref      . ,u8vector-ref)
    (u8vector?         . ,u8vector?)
    (u8vector-length   . ,u8vector-length)
    (boolean?          . ,boolean?)
    (bitwise-ior       . ,bitwise-ior)
    (bitwise-xor       . ,bitwise-xor)))

(for ([(name folder) (in-pairs folders)])
  (add-constant-folder name folder))
