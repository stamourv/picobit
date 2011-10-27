;; opt test for left-left-lambda inlining

(displayln ((lambda (x) (+ x 2)) 3)) ; yes

(define y 5)
(displayln ((lambda (x) (+ x 2)) y)) ; yes

(displayln ((lambda (x y) (+ x y 2)) 2 y)) ; yes

(displayln ((lambda (x y) (+ x 2)) 2 (displayln 3))) ; no, side-effect

(let ([x 3])
  (displayln (+ x 2))) ; yes

(let* ([x 3] ; yes
       [y 4]) ; chain. body of one l-l-l is another.
  (displayln (+ x y)))

;; can't mutate to fool other opts, mutable vars are not side-effect oblivious
(define z 4)
(displayln ((lambda (x) (+ x 3)) (#%+ 1 z))) ; yes, side-effect-oblivious

(define (f x) (#%+ 1 x))
(displayln ((lambda (x) (+ x 3)) (f z))) ; yes, body of f is ok
(displayln ((lambda (x) (+ x 3)) (+ 1 z))) ; no, body of + is recursive

(displayln ((lambda (x) (+ x 3)) (if z 2 3))) ; yes

(displayln ((lambda (x) (+ x 3)) (begin 2 3))) ; yes


(displayln ((lambda (x) (+ x (* x 2))) 3)) ; yes, trivial arg used twice
(displayln ((lambda (x) (+ x (* x 2)))
            (#%+ 1 z))) ; no, non-trivial arg used twice
;; Note: currently, the arg is only non-trivial because left-left-lambda
;; inlining is done before constant propagation. _Very_ brittle.
