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
