#!/usr/bin/env gsi
;; -*- scheme -*-

;; --- library stuff

(define (skip-head-match orig match)
  (let lp ((orig* orig)
	   (match* match))
    (if (null? match*)
	orig*
	(if (null? orig*)
	    orig
	    (if (equal? (car orig*)
			(car match*))
		(lp (cdr orig*)
		    (cdr match*))
		orig)))))

(define (basepath str)
  (list->string
   (reverse
    (skip-head-match
     (reverse (string->list str))
     (reverse (string->list ".scm"))))))

;; quick solution just differentiating between signals and exits.
;; todo: is even this part portable? probably not.
;;  (and further explanations are missing anyway)
(define (status->string s)
  (cond ((zero? s)
	 "exited successfully")
	((> s 255)
	 (string-append "exited with exit code "
			(number->string (arithmetic-shift s -8))))
	((< s 0)
	 (error "negative status" s))
	((string-append "terminated by signal "
			(number->string s)))))
;;^ doesn't check for the case where both exit code and signal is non-zero.


(define (run cmd . args)
  (let ((p (open-process (list path: (path-expand cmd)
			       arguments: args
			       stdin-redirection: #f
			       stdout-redirection: #f
			       stderr-redirection: #f))))
    (let ((s (process-status p)))
      (close-port p)
      (or (= s 0)
	  ;;(error )
	  (error (string-append "command "
				(status->string s)
				":")
		 cmd
		 args)))))

;; --- / library stuff


(define prog (car (command-line)))
(define args (cdr (command-line)))

(define (usage maybe-err)
  (println
   (list
    (if maybe-err
	(list maybe-err "\n\n")
	'())
    "usage: "prog" path/to/file.scm"))
  (newline)
  (exit 1))

(if (= (length args) 1)
    (let ((base (basepath (car args))))
      (run "picobit" (string-append base ".scm"))
      (run "picobit-vm" (string-append base ".hex")))
    (usage "wrong number of arguments"))

