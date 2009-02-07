(define-macro (test-cond condition name) ;; TODO watch out for name capture
  `(let ((DUMMY-x ,condition))
     (display ,name)
     (if DUMMY-x
       (display " : PASSED\n")
       (begin (display " : FAILED, got : ")
	      (display DUMMY-x)
	      (display "\n")))))

(test-cond (not (< 0 0)) "0 not neg")
(test-cond (< -1 0) "-1 neg")
(test-cond (= 5 5) "5 = 5")
(test-cond (< 2 5) "2 < 5")
(test-cond (> 5 2) "5 > 2")
(test-cond (= -5 -5) "-5 = -5")
(test-cond (> -2 -5) "-2 > -5")
(test-cond (< -5 -2) "-5 < -2")
(test-cond (< -5 65533) "-5 < 65533") ;; TODO still 16 bits, does the compiler encode it ok ? what about numbers over 16 bits long ?
(test-cond (< -5 2) "-5 < 2")
(test-cond (> 5 -65533) "5 > -65533")
(test-cond (> 5 -2) "5 > -2")
