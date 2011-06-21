(display (cond (#f 2) ((= 1 2) 4) (#t 2)))
;; if all is false, returns an object, acceptable

(define x 3)
(define y 2)
(set! x 5)

(display (cond ((> y x) "foo")
	       ((= 1 2) "bar")
	       (else "baz")))
(newline)
