;; bitwise or tests

(define-macro (test-cond condition name) ;; TODO watch out for name capture
  `(let ((DUMMY-x ,condition))
     (display ,name)
     (if DUMMY-x
	 (display " : PASSED\n")
	 (begin (display " : FAILED, got : ")
		(display DUMMY-x)
		(display "\n")))))

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

(test-= (bitwise-ior #xf0 #x0f) #xff "#xf0 | #x0f = #xff")
(test-= (bitwise-ior #x30 #x00) #x30 "#x30 | #x00 = #x30")
(test-= (bitwise-ior #x00 #x05) #x05 "#x00 | #x05 = #x05")
(test-= (bitwise-ior #x08 #x05) #x0D "#x08 | #x05 = #x0D")
(test-= (bitwise-ior #x18 #x05) #x1D "#x18 | #x05 = #x1D")
(test-= (bitwise-ior #x18 #x25) #x3D "#x18 | #x25 = #x3D")
(test-= (bitwise-ior #x18 #x35) #x3D "#x18 | #x35 = #x3D")
(test-= (bitwise-ior #x1823122312 #x351234123456) 58386709362518
	"#x1823122312 | #x351234123456 = 58386709362518")
