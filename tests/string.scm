;; tests with strings

(define (foo x)
  (+ (string-length x) 1))

(display "abc")
(display (foo "abc"))
(newline)
