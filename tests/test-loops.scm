;; tests with loops

(display (map (lambda (x) (+ x 1)) '(1 2 3 4 5)))
(display (for-each (lambda (x) (set! x (+ x 1))) '(1 2 3 4 5)))
