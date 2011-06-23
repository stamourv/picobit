;; tests with loops

(displayln (map (lambda (x) (+ x 1)) '(1 2 3 4 5)))
(displayln (for-each (lambda (x) (set! x (+ x 1))) '(1 2 3 4 5)))
