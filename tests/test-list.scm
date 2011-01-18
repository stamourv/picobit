;; tests with lists

(define lst1 '())
(define lst2 '(1 2 3 4))
(define lst3 '(a b c d))
(define lst4 '(#\a #\b #\c #\d))

(display (null? lst1))
(display (not (null? lst2)))
(display (length lst1))
(display (length lst2))
(display (append lst2 lst3))
(display (reverse lst4))

(display (list->string lst4))
(display (string->list "abcd"))

(list-set! lst2 2 5)
(display (list-ref lst2 2))
