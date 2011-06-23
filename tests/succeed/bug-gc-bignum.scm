;; This triggers gc inside bignum operations.
;; Before bignum temp vars were GC roots, this caused all kinds of
;; trouble.
;; Registering them as roots fixed the problem.
(displayln (* 1 3))
(displayln (* 15 3))
(displayln (* 21435 2141241))
