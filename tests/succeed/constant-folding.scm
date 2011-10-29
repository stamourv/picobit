;; For now, we have to use the primitives directly, since there's no way
;; to ensure that the + or * we're seeing is indeed the one from stdlib.
;; Also, the current test harness cannot detect if constant folding does
;; not happen. This test will only fail if there's an error somewhere.
(displayln (#%+ 2 (#%mul-non-neg 3 4)))

(displayln (if #t 1 2)) ; all these should be a single constant, no if
(displayln (if 3  1 2))
(displayln (if #f 1 2))

(displayln (begin 2 3)) ; same
(displayln (if (begin 2 3) 1 2))
