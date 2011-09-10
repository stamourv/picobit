(define light adc)

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
    (#%sleep-aux (#%+ (clock) duration))))

(define #%sleep-aux
  (lambda (wake-up)
    (if (< (clock) wake-up)
        (#%sleep-aux wake-up)
        #f)))


(define led2-color
  (lambda (state)
    (if (eq? state 'red)
        (#%led2-color 1)
        (#%led2-color 0))))

(define display
  (lambda (x)
    (if (string? x)
        (for-each putchar (string->list x))
        (write x))))

(define (newline) (#%putchar #\newline 3))

(define (displayln x) (display x) (newline))

(define write
  (lambda (x)
    (cond ((string? x)
	   (begin (#%putchar #\" 3)
		  (display x)
		  (#%putchar #\" 3)))
	  ((number? x)
	   (display (number->string x)))
	  ((pair? x)
	   (begin (#%putchar #\( 3)
                  (write (car x))
                  (#%write-list (cdr x))))
	  ((symbol? x)
	   (display "#<symbol>"))
	  ((boolean? x)
	   (display (if x "#t" "#f")))
	  (else
	   (display "#<object>")))))
;; TODO have vectors and co ?

(define #%write-list
  (lambda (lst)
    (cond ((null? lst)
	   (#%putchar #\) 3))
	  ((pair? lst)
	   (begin (#%putchar #\space 3)
		  (write (car lst))
		  (#%write-list (cdr lst))))
	  (else
	   (begin (display " . ")
		  (write lst)
		  (#%putchar #\) 3))))))

(define pp
  (lambda (x)
    (write x)
    (#%putchar #\newline 3)))

(define current-time clock)
(define time->seconds (lambda (t) (quotient t 100)))
