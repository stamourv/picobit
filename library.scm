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
    (or (< x y) (= x y))))

(define >
  (lambda (x y)
    (#%> x y)))

(define >=
  (lambda (x y)
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

(define string->list
  (lambda (str)
    (#%string->list str)))

(define list->string
  (lambda (chars)
    (#%list->string chars)))

(define string-length ;; TODO are all these string operations efficient ? they all convert to lists. use true vectors when we have them ?
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

(define beep
  (lambda (freq-div duration)
    (#%beep freq-div duration)))

(define light
  (lambda (sensor)
    (#%adc sensor)))

(define adc
  (lambda (sensor)
    (#%adc sensor)))

(define dac
  (lambda (level)
    (#%dac level)))

(define sernum
  (lambda ()
    (#%sernum)))

(define putchar
  (lambda (c)
    (#%putchar c 3)))

(define getchar
  (lambda ()
    (or (#%getchar-wait 0 3)
        (getchar))))

(define getchar-wait
  (lambda (duration)
    (#%getchar-wait duration 3)))

(define sleep
  (lambda (duration)
    (#%sleep-aux (#%+ (#%clock) duration))))

(define #%sleep-aux
  (lambda (wake-up)
    (if (#%< (#%clock) wake-up)
        (#%sleep-aux wake-up)
        #f)))

(define motor
  (lambda (id power)
    (#%motor id power)))


(define led
  (lambda (id duty period)
    (#%led id duty period)))

(define led2-color
  (lambda (state)
    (if (#%eq? state 'red)
        (#%led2-color 1)
        (#%led2-color 0))))

(define display
  (lambda (x)
    (if (#%string? x)
        (for-each putchar (#%string->list x))
        (write x))))

(define write
  (lambda (x)
    (if (#%string? x)
        (begin
          (#%putchar #\" 3)
          (display x)
          (#%putchar #\" 3))
        (if (#%number? x)
            (display (number->string x))
            (if (#%pair? x)
                (begin
                  (#%putchar #\( 3)
                  (write (#%car x))
                  (#%write-list (#%cdr x)))
                (if (#%symbol? x)
                    (display "#<symbol>")
                    (display "#<object>")))))))

(define #%write-list
  (lambda (lst)
    (if (#%null? lst)
        (#%putchar #\) 3)
        (if (#%pair? lst)
            (begin
              (#%putchar #\space 3)
              (write (#%car lst))
              (#%write-list (#%cdr lst)))
            (begin
              (display " . ")
              (write lst)
              (#%putchar #\) 3))))))

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
    (#%putchar #\newline 3)))

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
  (lambda (x y) ;; TODO rewrite once we have cond, also add vectors
    (if (eq? x y)
	#t
	(if (and (pair? x) (pair? y))
	    (and (equal? (car x) (car y))
		 (equal? (cdr x) (cdr y)))
	    #f)))) ;; TODO could this have a problem ?

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

(define bitwise-ior (lambda (x y) (#%ior x y)))
(define bitwise-xor (lambda (x y) (#%xor x y)))
;; TODO add bitwise-and ? bitwise-not ?

(define current-time (lambda () (clock)))
(define time->seconds (lambda (t) (quotient t 100))) ;; TODO no floats, is that a problem ?

(define else #t) ; for cond, among others

;; TODO temporary, using lists since triplets are gone
(define u8vector (lambda x (list x)))
(define list->u8vector (lambda (x) x))
(define u8vector-length (lambda (x) (length x)))
(define u8vector-ref (lambda (x y) (list-ref x y)))
(define u8vector-set! (lambda (x y z) (list-set! x y z)))
(define make-u8vector
  (lambda (n x)
    (if (= n 0)
	'()
	(cons x (make-u8vector (- n 1) x)))))


;; ROM VECTORS
;; TODO make sure constant vectors end up in rom
;; (define u8vector ;; TODO use chris okasaki's random access lists for mutable vectors, and in-rom vectors (strings) for the rest, these functions are for the in-rom vectors
;;   (lambda (first . rest) ;; TODO can't we have all in the same arg ?
;;     (list->u8vector (cons first rest))))
;; ;; TODO maybe still use the parser hack for the in-rom vectors, since they are known at compile time (but some might have variables inside instead of only numbers, would not work then)

;; (define u8vector-ref
;;   (lambda (u8 i)
;;     (#%car (#%substring-aux1 (#%string->list u8) i))))
;; ;; TODO yuck, this is O(n), do better, since we have contiguous memory for in-rom vectors, but not that important since these rom vectors are all small

(define print display) ;; TODO watch out for differences between the 2
