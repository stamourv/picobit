;; very simple sanity test for bignums
(define (foo x y)
  (+ x y))

;(foo 1 3)
(display (if (= (foo 1 3) 4) ; TODO test with bignum-range numbers
	     "OK\n"
	     "NAH\n"))
