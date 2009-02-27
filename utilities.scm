;;;; File: "utilities.scm", Time-stamp: <2006-05-08 16:04:37 feeley>

;;;; Copyright (C) 2004-2009 by Marc Feeley and Vincent St-Amour
;;;; All Rights Reserved.

(define keep
  (lambda (keep? lst)
    (cond ((null? lst)       '())
          ((keep? (car lst)) (cons (car lst) (keep keep? (cdr lst))))
          (else              (keep keep? (cdr lst))))))

(define take
  (lambda (n lst)
    (if (> n 0)
        (cons (car lst) (take (- n 1) (cdr lst)))
        '())))

(define drop
  (lambda (n lst)
    (if (> n 0)
        (drop (- n 1) (cdr lst))
        lst)))

(define repeat
  (lambda (n x)
    (if (> n 0)
        (cons x (repeat (- n 1) x))
        '())))

(define pos-in-list
  (lambda (x lst)
    (let loop ((lst lst) (i 0))
      (cond ((not (pair? lst)) #f)
            ((eq? (car lst) x) i)
            (else              (loop (cdr lst) (+ i 1)))))))

(define every
  (lambda (pred? lst)
    (or (null? lst)
        (and (pred? (car lst))
             (every pred? (cdr lst))))))


(define (sort-list l <?)

  (define (mergesort l)

    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (<? e1 e2)
                 (cons e1 (merge (cdr l1) l2))
                 (cons e2 (merge l1 (cdr l2))))))))

    (define (split l)
      (if (or (null? l) (null? (cdr l)))
        l
        (cons (car l) (split (cddr l)))))

    (if (or (null? l) (null? (cdr l)))
      l
      (let* ((l1 (mergesort (split l)))
             (l2 (mergesort (split (cdr l)))))
        (merge l1 l2))))

  (mergesort l))
