;; test to see if binary arithmetic operator optimisation breaks anything
;; now a test to see if the new bignums break arithmetic
(define (foo x y z)
  (* 2 3 (+ x 3) (- y 2) (+ z 1))) ;; TODO OK everything works for that now

;; (define (foo x y z)
;;   (+ 80 (* 2 3 (+ 2 x) (- z 5)))) ; TODO 200 didn't seem to be encoded as an int, or maybe the final 254...
;; TODO +50 or +70 is ok, but not +100 or +80 which give decode_int8 errors

(display (if (= (foo 1 3 8) 216)
 ; (if (= (foo 1 3 8) 216)
 ; (if (= (foo 1 3 8) 134)
	     "OK\n"
	     "NAH\n"))

;; TODO weird, with the original test (216), the wrong result was given, but no decode_int8 was given
