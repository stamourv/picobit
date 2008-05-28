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
    (#%<= x y)))

(define >
  (lambda (x y)
    (#%> x y)))

(define >=
  (lambda (x y)
    (#%>= x y)))

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
        (#%length-aux lst (#%+ n 1))
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
    (if (#%>= n 1) ;; TODO had an off-by-one
        (#%substring-aux1 (#%cdr lst) (#%- n 1))
        lst)))

(define #%substring-aux2
  (lambda (lst n)
    (if (#%>= n 1) ;; TODO had an off-by-one
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

(define u8vector-ref ;; ADDED
  (lambda (u8 i)
    (#%car (#%substring-aux1 (#%string->list u8) i))))

(define u8vector-set! ;; ADDED
  (lambda (u8 i val)
    ))

;; TODO u8vector-set!
