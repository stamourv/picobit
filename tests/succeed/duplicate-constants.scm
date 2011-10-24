;; this is a test that ensures that multiple copies of the same constant
;; are generated only one in the resulting program
;; this has to be checked manually
(define x '(1 2 3))
(define y '(1 2 3))
(displayln (append x y))
