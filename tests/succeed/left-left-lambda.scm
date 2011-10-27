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

(define z 3)
(set! z 4) ; to fool copy-propagation and constant folding
(displayln ((lambda (x) (+ x 3)) (#%+ 1 z))) ; yes, side-effect-less? call

(define (f x) (#%+ 1 x))
(displayln ((lambda (x) (+ x 3)) (f z))) ; yes, body of f is ok
(displayln ((lambda (x) (+ x 3)) (+ 1 z))) ; no, body of + is recursive

(displayln ((lambda (x) (+ x 3)) (if z 2 3))) ; yes

(displayln ((lambda (x) (+ x 3)) (begin 2 3))) ; yes
