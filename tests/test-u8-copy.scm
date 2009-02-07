;; test for the 2 cases of vector copy
;; TODO error case ?
(define x '#u8(0 1 2 3 4 5))
(define y (u8vector 10 11 12 13 14 15 16 17 18 19))
(define z (u8vector 20 21 22 23 24 25 26 27 28 29))

(define (display-vector v)
  (let loop ((i 0))
    (if (< i (u8vector-length v))
	(begin (display (u8vector-ref v i))
	       (display " ")
	       (loop (+ i 1)))))
  (display "\n"))

;; (display-vector x)
;; (display-vector y)
;; (display-vector z)

(u8vector-copy! x 1 y 3 4)
(u8vector-copy! y 2 z 4 5)
(u8vector-copy! z 0 z 2 2)
(display-vector x)
(display-vector y)
(display-vector z)
