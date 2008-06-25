; File: "library.scm"

(define number?
  (lambda (x)
    (#%number? x)))

(define +
  (lambda (x . rest)
    (if (#%pair? rest)
        (#%+-aux (#%+ x (#%car rest)) (#%cdr rest))
        x)))

(define #%+-aux
  (lambda (x rest)
    (if (#%pair? rest)
        (#%+-aux (#%+ x (#%car rest)) (#%cdr rest))
        x)))

(define -
  (lambda (x . rest)
    (if (#%pair? rest)
        (#%--aux (#%- x (#%car rest)) (#%cdr rest))
        (#%neg x))))

(define #%--aux
  (lambda (x rest)
    (if (#%pair? rest)
        (#%--aux (#%- x (#%car rest)) (#%cdr rest))
        x)))

(define *
  (lambda (x . rest)
    (if (#%pair? rest)
        (#%*-aux (#%* x (#%car rest)) (#%cdr rest))
        x)))

(define #%*-aux
  (lambda (x rest)
    (if (#%pair? rest)
        (#%*-aux (#%* x (#%car rest)) (#%cdr rest))
        x)))

(define quotient
  (lambda (x y)
    (#%quotient x y)))

(define remainder
  (lambda (x y)
    (#%remainder x y)))

(define =
  (lambda (x y)
    (#%= x y)))

(define <
  (lambda (x y)
    (#%< x y)))

(define <=
  (lambda (x y)
    ;; (#%<= x y) ;; ADDED not a primitive anymore
    (or (< x y) (= x y))))

(define >
  (lambda (x y)
    (#%> x y)))

(define >=
  (lambda (x y)
    ;; (#%>= x y) ;; ADDED, not a primitive anymore
    (or (> x y) (= x y))))

(define pair?
  (lambda (x)
    (#%pair? x)))

(define cons
  (lambda (x y)
    (#%cons x y)))

(define car
  (lambda (x)
    (#%car x)))

(define cdr
  (lambda (x)
    (#%cdr x)))

(define set-car!
  (lambda (x y)
    (#%set-car! x y)))

(define set-cdr!
  (lambda (x y)
    (#%set-cdr! x y)))

(define null?
  (lambda (x)
    (#%null? x)))

(define eq?
  (lambda (x y)
    (#%eq? x y)))

(define not
  (lambda (x)
    (#%not x)))

(define list
  (lambda lst lst))

(define length
  (lambda (lst)
    (#%length-aux lst 0)))

(define #%length-aux
  (lambda (lst n)
    (if (#%pair? lst)
        (#%length-aux (cdr lst) (#%+ n 1)) ;; TODO had an error and looped
        n)))

(define append
  (lambda (lst1 lst2)
    (if (#%pair? lst1)
        (#%cons (#%car lst1) (append (#%cdr lst1) lst2))
        lst2)))

(define reverse
  (lambda (lst)
    (reverse-aux lst '())))

(define reverse-aux
  (lambda (lst rev)
    (if (#%pair? lst)
        (reverse-aux (#%cdr lst) (#%cons (#%car lst) rev))
        rev)))

(define list-ref
  (lambda (lst i)
    (if (#%= i 0)
        (#%car lst)
        (list-ref (#%cdr lst) (#%- i 1)))))

(define list-set!
  (lambda (lst i x)
    (if (#%= i 0)
        (#%set-car! lst x)
        (list-set! (#%cdr lst) (#%- i 1) x))))

(define max
  (lambda (x y)
    (if (#%> x y) x y)))

(define min
  (lambda (x y)
    (if (#%< x y) x y)))

(define abs
  (lambda (x)
    (if (#%< x 0) (#%neg x) x)))

(define modulo
  (lambda (x y)
    (#%remainder x y)))

(define string
  (lambda chars
    (#%list->string chars)))

(define string-length ;; TODO are all these string operations efficient ? they all convert to lists. Since we have the equivalent of a vector, isn't there a way to do better ?
  (lambda (str)
    (length (#%string->list str))))

(define string-append
  (lambda (str1 str2)
    (#%list->string (append (#%string->list str1) (#%string->list str2)))))

(define substring
  (lambda (str start end)
    (#%list->string
     (#%substring-aux2
      (#%substring-aux1 (#%string->list str) start)
      (#%- end start)))))

(define #%substring-aux1
  (lambda (lst n)
    (if (>= n 1) ;; TODO had an off-by-one
        (#%substring-aux1 (#%cdr lst) (#%- n 1))
        lst)))

(define #%substring-aux2
  (lambda (lst n)
    (if (>= n 1) ;; TODO had an off-by-one
        (#%cons (#%car lst) (#%substring-aux2 (#%cdr lst) (#%- n 1)))
        '())))

(define map
  (lambda (f lst)
    (if (#%pair? lst)
        (#%cons (f (#%car lst))
                (map f (#%cdr lst)))
        '())))

(define for-each
  (lambda (f lst)
    (if (#%pair? lst)
        (begin
          (f (#%car lst))
          (for-each f (#%cdr lst)))
        #f)))

(define call/cc
  (lambda (receiver)
    (let ((k (#%get-cont)))
      (receiver
       (lambda (r)
         (#%return-to-cont k r))))))

(define root-k #f)
(define readyq #f)

(define start-first-process
  (lambda (thunk)
    (set! root-k (#%get-cont))
    (set! readyq (#%cons #f #f))
    (#%set-cdr! readyq readyq)
    (thunk)))

(define spawn
  (lambda (thunk)
    (let* ((k (#%get-cont))
           (next (#%cons k (#%cdr readyq))))
      (#%set-cdr! readyq next)
      (#%graft-to-cont root-k thunk))))

(define exit
  (lambda ()
    (let ((next (#%cdr readyq)))
      (if (#%eq? next readyq)
          (#%halt)
          (begin
            (#%set-cdr! readyq (#%cdr next))
            (#%return-to-cont (#%car next) #f))))))

(define yield
  (lambda ()
    (let ((k (#%get-cont)))
      (#%set-car! readyq k)
      (set! readyq (#%cdr readyq))
      (let ((next-k (#%car readyq)))
        (#%set-car! readyq #f)
        (#%return-to-cont next-k #f)))))

(define clock
  (lambda ()
    (#%clock)))

(define light
  (lambda ()
    (#%light)))

(define putchar
  (lambda (c)
    (#%putchar c)))

(define getchar
  (lambda ()
    (or (#%getchar-wait 0)
        (getchar))))

(define getchar-wait
  (lambda (duration)
    (#%getchar-wait duration)))

(define sleep
  (lambda (duration)
    (#%sleep-aux (#%+ (#%clock) duration))))

(define #%sleep-aux
  (lambda (wake-up)
    (if (#%< (#%clock) wake-up)
        (#%sleep-aux wake-up)
        #f)))

(define motor
  (lambda (x y z)
    (#%motor x y z)))

(define led
  (lambda (state)
    (if (#%eq? state 'red)
        (#%led 1)
        (if (#%eq? state 'green)
            (#%led 2)
            (#%led 0)))))

(define display
  (lambda (x)
    (if (#%string? x)
        (for-each putchar (#%string->list x))
        (write x))))

(define write
  (lambda (x)
    (if (#%string? x)
        (begin
          (#%putchar #\")
          (display x)
          (#%putchar #\"))
        (if (#%number? x)
            (display (number->string x))
            (if (#%pair? x)
                (begin
                  (#%putchar #\()
                  (write (#%car x))
                  (#%write-list (#%cdr x)))
                (if (#%symbol? x)
                    (display "#<symbol>")
                    (display "#<object>")))))))

(define #%write-list
  (lambda (lst)
    (if (#%null? lst)
        (#%putchar #\))
        (if (#%pair? lst)
            (begin
              (#%putchar #\space)
              (write (#%car lst))
              (#%write-list (#%cdr lst)))
            (begin
              (display " . ")
              (write lst)
              (#%putchar #\)))))))

(define number->string
  (lambda (n)
    (#%list->string
     (if (#%< n 0)
         (#%cons #\- (#%number->string-aux (#%neg n) '()))
         (#%number->string-aux n '())))))

(define #%number->string-aux
  (lambda (n lst)
    (let ((rest (#%cons (#%+ #\0 (#%remainder n 10)) lst)))
      (if (#%< n 10)
          rest
          (#%number->string-aux (#%quotient n 10) rest)))))

(define pp
  (lambda (x)
    (write x)
    (#%putchar #\newline)))

(define caar
  (lambda (p)
    (car (car p))))
(define cadr
  (lambda (p)
    (car (cdr p))))
(define cdar
  (lambda (p)
    (cdr (car p))))
(define cddr ;; TODO implement all of them up to 4 chars ?
  (lambda (p)
    (cdr (cdr p))))
(define caadr
  (lambda (p)
    (car (car (cdr p)))))
(define cdadr
  (lambda (p)
    (cdr (car (cdr p)))))

(define equal?
  (lambda (x y) ;; TODO rewrite once we have cond
    (if (eq? x y)
	#t
	(if (and (pair? x) (pair? y))
	    (and (equal? (car x) (car y))
		 (equal? (cdr x) (cdr y)))
	    (if (and (triplet? x) (triplet? y))
		(and (equal? (fst x) (fst y))
		     (equal? (snd x) (snd y))
		     (equal? (trd x) (trd y)))
		#f))))) ;; TODO could this have a problem ?

(define assoc
  (lambda (t l) ;; TODO rewrite once we have cond
    (if (null? l)
	#f
	(if (equal? t (caar l))
	    (car l)
	    (assoc t (cdr l))))))

;; TODO ordinary vectors are never more that 6 elements long in the stack, so implementing them as lists is acceptable
(define vector list)
(define vector-ref list-ref)
(define vector-set! list-set!)

(define triplet? (lambda (t) (#%triplet? t)))
(define triplet (lambda (x y z) (#%triplet x y z)))
(define fst (lambda (t) (#%fst t)))
(define snd (lambda (t) (#%snd t)))
(define trd (lambda (t) (#%trd t)))
(define set-fst! (lambda (t v) (#%set-fst! t v)))
(define set-snd! (lambda (t v) (#%set-snd! t v)))
(define set-trd! (lambda (t v) (#%set-trd! t v)))
;; TODO for tests on gambit
;; (define (triplet x y z) (vector x y z))
;; (define (fst t) (vector-ref t 0))
;; (define (snd t) (vector-ref t 1))
;; (define (trd t) (vector-ref t 2))
;; (define (set-fst! t v) (vector-set! t 0 v))
;; (define (set-snd! t v) (vector-set! t 1 v))
;; (define (set-trd! t v) (vector-set! t 2 v))


(define bitwise-ior (lambda (x y) (#%ior x y)))
(define bitwise-xor (lambda (x y) (#%xor x y)))
;; TODO add bitwise-and ? bitwise-not ?

(define current-time (lambda () (clock)))
(define time->seconds (lambda (t) (quotient t 100))) ;; TODO no floats, is that a problem ?

(define else #t) ; for cond, among others

;; vectors are implemented using r-a-lists
;; TODO takes only marginally more code space than lists made from triplets, maybe 150 bytes more in the stack (the total is in the order of 10.5k)
(define u8vector (lambda x (list->u8vector x)))
(define list->u8vector (lambda (x) (list->r-a-list x)))
(define u8vector-length (lambda (x) (r-a-length x)))
(define u8vector-ref (lambda (x y) (r-a-ref x y)))
(define u8vector-set! (lambda (x y z) (r-a-set! x y z)))
(define make-u8vector
  (lambda (n x)
    (if (= n 0)
	'()
	(r-a-cons x (make-u8vector (- n 1) x)))))


;; implementation of Chris Okasaki's random access lists
;; basically, we have a list (made from pairs) of pairs of complete binary
;; trees (made from triplets) and their number of elements (length first)
;; the trees are represented : (root left right)
;; however, unlike Okasaki, our lists are not purely functional, since we do
;; the changes in-place

(define r-a-list (lambda x (list->r-a-list x)))
(define list->r-a-list
  (lambda (l) (if (null? l) '() (r-a-cons (car l) (list->r-a-list (cdr l))))))

(define r-a-cons
  (lambda (x y)
    (if (and (pair? y)
	     (pair? (cdr y))
	     (= (caar y) (caadr y)))
	;; the first 2 trees are of the same size, merge them
	(cons (cons (+ 1 (caar y) (caadr y))
		    (triplet x (cdar y) (cdadr y)))
	      (cddr y))
	;; the first 2 trees are not of the same size, insert in front
	(cons (cons 1 (triplet x '() '()))
	      y))))

(define r-a-length
  (lambda (l) (if (null? l) 0 (+ (caar l) (r-a-length (cdr l))))))

(define r-a-ref
  (lambda (r i)
    (if (null? r)
	#f ; out of bounds
	(let ((size (caar r)))
	  (if (< i size)
	      ;; what we want is in the 1st tree
	      (r-a-tree-ref size (cdar r) i)
	      ;; keep looking
	      (r-a-ref (cdr r) (- i size)))))))
(define r-a-tree-ref
  (lambda (s r i)
    (if (= i 0)
	(fst r)
	(let ((s2 (quotient s 2)))
	  (if (<= i s2)
	      ;; these 2 will break if the tree is malformed
	      (r-a-tree-ref s2 (snd r) (- i 1))
	      (r-a-tree-ref s2 (trd r) (- i 1 s2)))))))

(define r-a-set! ; unlike Okasaki, we do the change in-place
  (lambda (r i v)
    (if (null? r)
	#f ; out of bounds
	(let ((size (caar r)))
	  (if (< i size)
	      ;; what we want is in the 1st tree
	      (r-a-tree-set! size (cdar r) i v)
	      ;; keep looking
	      (r-a-set! (cdr r) (- i size) v))))))
(define r-a-tree-set!
  (lambda (s r i v)
    (if (= i 0)
	(set-fst! r v)
	(let ((s2 (quotient s 2)))
	  (if (<= i s2)
	      ;; these 2 will break if the tree is malformed
	      (r-a-tree-set! s2 (snd r) (- i 1) v)
	      (r-a-tree-set! s2 (trd r) (- i 1 s2) v))))))


;; ROM VECTORS
;; (define u8vector ;; TODO use chris okasaki's random access lists for mutable vectors, and in-rom vectors (strings) for the rest, these functions are for the in-rom vectors
;;   (lambda (first . rest) ;; TODO can't we have all in the same arg ?
;;     (list->u8vector (cons first rest))))
;; ;; TODO maybe still use the parser hack for the in-rom vectors, since they are known at compile time (but some might have variables inside instead of only numbers, would not work then)

;; (define u8vector-ref
;;   (lambda (u8 i)
;;     (#%car (#%substring-aux1 (#%string->list u8) i))))
;; ;; TODO yuck, this is O(n), do better, since we have contiguous memory for in-rom vectors, but not that important since these rom vectors are all small

(define print display) ;; TODO watch out for differences between the 2
