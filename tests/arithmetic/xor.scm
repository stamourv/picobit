;; bitwise xor tests

(define-macro (test-= lhs rhs name) ;; TODO watch out for name capture
  `(let ((DUMMY-lhs ,lhs)
	 (DUMMY-rhs ,rhs))
     (display ,name)
     (if (= DUMMY-lhs DUMMY-rhs)
	 (display " : PASSED\n")
	 (begin (display " : FAILED, got : ")
		(display DUMMY-lhs)
		(display " and ")
		(display DUMMY-rhs)
		(display "\n")))))

(test-= (bitwise-xor #xf0 #x0f) #xff "#xf0 | #x0f = #xff")
(test-= (bitwise-xor #x30 #x00) #x30 "#x30 | #x00 = #x30")
(test-= (bitwise-xor #x00 #x05) #x05 "#x00 | #x05 = #x05")
(test-= (bitwise-xor #x08 #x05) #x0D "#x08 | #x05 = #x0D")
(test-= (bitwise-xor #x18 #x05) #x1D "#x18 | #x05 = #x1D")
(test-= (bitwise-xor #x18 #x25) #x3D "#x18 | #x25 = #x3D")
(test-= (bitwise-xor #x18 #x35) #x2D "#x18 | #x35 = #x2D")
(test-= (bitwise-xor #xF7 #xDD) #x2A "#xF7 | #xDD = #x2A")
