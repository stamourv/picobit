#lang racket

(require "ir.rkt" "back-end.rkt")

(provide schedule)

;; Basic block scheduling.
(define (schedule bbs)
  (linearize (reorder! bbs)))


;-----------------------------------------------------------------------------

(define (reorder! bbs)
  (let* ([done (make-vector (vector-length bbs) #f)])

    (define (unscheduled? label) (not (vector-ref done label)))

    (define (label-refs instrs todo)
      (for/fold ([todo todo])
          ([instr (in-list instrs)]
           #:when (memq (car instr)'(closure call-toplevel jump-toplevel)))
        (cons (cadr instr) todo)))

    (define (schedule-here label new-label todo)
      (match (vector-ref bbs label)
        [(bb label (and rev-instrs `(,jump . ,rest)))
         (define new-todo (label-refs rev-instrs todo))
         (vector-set! bbs  label (make-bb new-label rev-instrs))
         (vector-set! done label #t)
         (match jump
           [`(goto ,label)
            (if (unscheduled? label)
                (schedule-here label (+ new-label 1) new-todo)
                (values (+ new-label 1) new-todo))]
           [`(goto-if-false ,label-then ,label-else)
            (cond [(unscheduled? label-else)
                   (schedule-here label-else
                                  (+ new-label 1)
                                  (cons label-then new-todo))]
                  [(unscheduled? label-then)
                   (schedule-here label-then
                                  (+ new-label 1)
                                  new-todo)]
                  [else (values (+ new-label 1) new-todo)])]
           [_ (values (+ new-label 1) new-todo)])]))

    (define (schedule-todo new-label todo)
      (when (pair? todo)
        (let ([label (car todo)])
          (if (unscheduled? label)
              (call-with-values
                  (lambda () (schedule-here label new-label (cdr todo)))
                schedule-todo)
              (schedule-todo new-label (cdr todo))))))

    (call-with-values (lambda () (schedule-here 0 0 '()))
      schedule-todo)

    (define len (vector-length bbs))
    (renumber-labels bbs (make-vector len 1) len)))

;-----------------------------------------------------------------------------

;; State for linearize.
;; Ugly, but better than having everything as an internal define
(define rev-code '())
(define pos 0)
(define todo (mcons '() '()))
(define bbs #f)
(define dumped #f)

(define (emit x)
  (set! pos (+ pos 1))
  (set! rev-code (cons x rev-code)))

(define (get fallthrough-to-next?)
  (define r-todo (mcdr todo))
  (and (mpair? r-todo)
       (if fallthrough-to-next?
           (match r-todo
             [(mcons (and label-pos `(,label . ,_)) rest )
              (unless (mpair? rest)
                (set-mcar! todo todo))
              (set-mcdr! todo rest)
              label])
           (let ([best-label-pos
                  (for/fold ([best-label-pos #f])
                      ([x (in-mlist r-todo)]
                       #:when (not (vector-ref dumped (car x)))
                       #:when (or (not best-label-pos)
                                  (> (cdr x) (cdr best-label-pos))))
                    x)])
             (and best-label-pos
                  (car best-label-pos))))))

(define (next)
  (for/first ([label-pos (in-mlist (mcdr todo))]
              #:when (not (vector-ref dumped (car label-pos))))
    (car label-pos)))

(define (schedule! label tail?)
  (let ([label-pos (cons label pos)])
    (if tail?
        (let ([cell (mcons label-pos '())])
          (set-mcdr! (mcar todo) cell)
          (set-mcar! todo cell))
        (let ([cell (mcons label-pos (mcdr todo))])
          (set-mcdr! todo cell)
          (when (eq? (mcar todo) todo)
            (set-mcar! todo cell))))))

(define (dump)
  (let loop ([fallthrough-to-next? #t])
    (let ([label (get fallthrough-to-next?)])
      (when label
        (cond [(not (vector-ref dumped label))
               (vector-set! dumped label #t)
               (loop (dump-bb label))]
              [else (loop fallthrough-to-next?)])))))

(define (dump-bb label)
  (match (vector-ref bbs label)
    [(bb label `(,jump . ,rest))
     (emit label)
     (for ([instr (in-list (reverse rest))])
       (match instr
         [`(,(or 'closure 'call-toplevel) ,arg)
          (schedule! arg #t)]
         [_ (void)])
       (emit instr))
     (match jump
       [`(goto ,label)
        (schedule! label #f)
        (if (not (equal? label (next)))
            (begin (emit jump) #f)
            #t)]
       [`(goto-if-false ,label-then ,label-else)
        (schedule! label-then #f)
        (schedule! label-else #f)
        (cond [(equal? label-else (next))
               (emit (list 'goto-if-false label-then))
               #t]
              [(equal? label-then (next))
               (emit (list 'prim '#%not))
               (emit (list 'goto-if-false label-else))
               #t]
              [else
               (emit (list 'goto-if-false label-then))
               (emit (list 'goto label-else))
               #f])]
       [`(jump-toplevel ,label)
        (schedule! label #f)
        ;; it is not correct to remove jump-toplevel when label is next
        (emit jump)
        #f]
       [_
        (emit jump)
        #f])]))


(define (linearize cur-bbs)
  (set! bbs    cur-bbs)
  (set! dumped (make-vector (vector-length cur-bbs) #f))
  (set-mcar! todo todo) ;; make fifo
  (schedule! 0 #f)
  (dump)
  (reverse rev-code))
