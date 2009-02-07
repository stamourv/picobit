(define-macro (test-cond condition name) ;; TODO watch out for name capture
  `(let ((DUMMY-x ,condition))
     (display ,name)
     (if DUMMY-x
       (display " : PASSED\n")
       (begin (display " : FAILED, got : ")
	      (display DUMMY-x)
	      (display "\n")))))

(test-cond (= (- 2 2) 0) "2 - 2 = 0")
(test-cond (= (- 5 3) 2) "5 - 3 = 0")
(test-cond (= (- 5 7) -2) "5 - 7 = -2")
(test-cond (= (- 4 0) 4) "4 - 0 = 4")
(test-cond (= (- 0 4) -4) "0 - 4 = -4")
(test-cond (= (- 2 -2) 4) "2 - -2 = 4")
(test-cond (= (- -2 -5) 3) "-2 - -5 = 3")
(test-cond (= (- -5 -2) -3) "-5 - -2 = -3")
