(define-macro (test-cond condition name) ;; TODO watch out for name capture
  `(let ((DUMMY-x ,condition))
     (display ,name)
     (if DUMMY-x
       (display " : PASSED\n")
       (begin (display " : FAILED, got : ")
	      (display DUMMY-x)
	      (display "\n")))))

(test-cond (= (+ 2 2) 4) "2 + 2 = 4")
(test-cond (= (+ 0 5) 5) "0 + 5 = 5")
(test-cond (= (+ 2 7) 9) "2 + 7 = 9")
(test-cond (= (+ 0 -5) -5) "0 + -5 = -5")
(test-cond (= (+ 2 -5) -3) "2 + -5 = -3")
(test-cond (= (+ 7 -5) 2) "7 + -5 = 2")
(test-cond (= (+ -3 -7) -10) "-3 + -7 = -10")

(test-cond (= (+ 100000 1) (+ 50000 50001)) "100000 + 1 = 50000 + 50001")
(test-cond (= (+ 32768 32768) 65536) "32k + 32k = 64k")
