(define-macro (test-cond condition name) ;; TODO watch out for name capture
  `(let ((DUMMY-x ,condition))
     (display ,name)
     (if DUMMY-x
       (display " : PASSED\n")
       (begin (display " : FAILED, got : ")
	      (display DUMMY-x)
	      (display "\n")))))

(test-cond (= (* 2 2) 4) "2 * 2 = 4")
(test-cond (= (* 0 5) 0) "0 * 5 = 0")
(test-cond (= (* 2 7) 14) "2 * 7 = 14")
(test-cond (= (* 1 -5) -5) "1 * -5 = -5")
(test-cond (= (* 2 -5) -10) "2 * -5 = -10")
(test-cond (= (* -7 5) -35) "-7 * 5 = -35")
(test-cond (= (* -3 -7) 21) "-3 * -7 = 21")

(test-cond (= (* 10000 10000) 100000000) "1e4 * 1e4 = 1e8")
(test-cond (= (* -10000 10000) -100000000) "-1e4 * 1e4 = -1e8")
(test-cond (= (* 10000 -10000) -100000000) "1e4 * -1e4 = -1e8")
(test-cond (= (* -10000 -10000) 100000000) "-1e4 * -1e4 = 1e8")

(test-cond (= (* 100000 10000) 1000000000) "1e5 * 1e4 = 1e9")
(test-cond (= (* 10000 100000) 1000000000) "1e4 * 1e5 = 1e9")
(test-cond (= (* 100000 100000) 10000000000) "1e5 * 1e5 = 1e10")
;; from that we can infer that the neg function (that negates, obviously) works
;; since it is used by multiplication when operands are negative
