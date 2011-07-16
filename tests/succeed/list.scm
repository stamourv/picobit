;; tests with lists

(define lst1 '())
(define lst2 '(1 2 3 4))
(define lst3 '(a b c d))
(define lst4 '(#\a #\b #\c #\d))

(displayln (null? lst1))
(displayln (not (null? lst2)))
(displayln (length lst1))
(displayln (length lst2))
(displayln (append lst2 lst3))
(displayln (reverse lst4))

(displayln (list->string lst4))
(define s "abcd") ; to defeat constant folding
(displayln (string->list s))

(define lst5 (list 1 2 3 4))
(list-set! lst5 2 5)
(displayln (list-ref lst5 2))
