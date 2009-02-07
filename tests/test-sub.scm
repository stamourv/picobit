;; very simple sanity test for bignums
(define (foo x y)
  (- x y))

;(foo 1 3)
(display (if (= (foo 15 3) 12) ; TODO test with bignum-range numbers
	     "OK\n"
	     "NAH\n"))
