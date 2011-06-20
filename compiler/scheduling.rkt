#lang racket

(provide (all-defined-out))

(require "ir.rkt" "back-end.rkt")

;; Basic block scheduling.

(define (reorder! bbs)
  (let* ((done (make-vector (vector-length bbs) #f)))

    (define unscheduled?
      (lambda (label)
        (not (vector-ref done label))))

    (define label-refs
      (lambda (instrs todo)
        (if (pair? instrs)
            (let* ((instr (car instrs))
                   (opcode (car instr)))
              (cond ((or (eq? opcode 'closure)
                         (eq? opcode 'call-toplevel)
                         (eq? opcode 'jump-toplevel))
                     (label-refs (cdr instrs) (cons (cadr instr) todo)))
                    (else
                     (label-refs (cdr instrs) todo))))
            todo)))

    (define schedule-here
      (lambda (label new-label todo cont)
        (let* ((bb (vector-ref bbs label))
               (rev-instrs (bb-rev-instrs bb))
               (jump (car rev-instrs))
               (opcode (car jump))
               (new-todo (label-refs rev-instrs todo)))
          (vector-set! bbs label (make-bb new-label rev-instrs))
          (vector-set! done label #t)
          (cond ((eq? opcode 'goto)
                 (let ((label (cadr jump)))
                   (if (unscheduled? label)
                       (schedule-here label
                                      (+ new-label 1)
                                      new-todo
                                      cont)
                       (cont (+ new-label 1)
                             new-todo))))
                ((eq? opcode 'goto-if-false)
                 (let ((label-then (cadr jump))
                       (label-else (caddr jump)))
                   (cond ((unscheduled? label-else)
                          (schedule-here label-else
                                         (+ new-label 1)
                                         (cons label-then new-todo)
                                         cont))
                         ((unscheduled? label-then)
                          (schedule-here label-then
                                         (+ new-label 1)
                                         new-todo
                                         cont))
                         (else
                          (cont (+ new-label 1)
                                new-todo)))))
                (else
                 (cont (+ new-label 1)
                       new-todo))))))

    (define schedule-somewhere
      (lambda (label new-label todo cont)
        (schedule-here label new-label todo cont)))

    (define schedule-todo
      (lambda (new-label todo)
        (when (pair? todo)
          (let ((label (car todo)))
            (if (unscheduled? label)
                (schedule-somewhere label
                                    new-label
                                    (cdr todo)
                                    schedule-todo)
                (schedule-todo new-label
                               (cdr todo)))))))


    (schedule-here 0 0 '() schedule-todo)

    (renumber-labels bbs
                     (make-vector (vector-length bbs) 1)
                     (vector-length bbs))))

;-----------------------------------------------------------------------------

(define (linearize bbs)
  (define rev-code '())

  (define pos 0)

  (define (emit x)
    (set! pos (+ pos 1))
    (set! rev-code (cons x rev-code)))

  (define todo (mcons '() '()))

  (define dumped (make-vector (vector-length bbs) #f))

  (define (get fallthrough-to-next?)
    (if (mpair? (mcdr todo))
        (if fallthrough-to-next?
            (let* ((label-pos (mcar (mcdr todo)))
                   (label (car label-pos))
                   (rest (mcdr (mcdr todo))))
              (unless (mpair? rest)
                (set-mcar! todo todo))
              (set-mcdr! todo rest)
              label)
            (let loop ((x (mcdr todo)) (best-label-pos #f))
              (if (mpair? x)
                  (loop (mcdr x)
                        (if (vector-ref dumped (car (mcar x)))
                            best-label-pos
                            (if (or (not best-label-pos)
                                    (> (cdr (mcar x)) (cdr best-label-pos)))
                                (mcar x)
                                best-label-pos)))
                  (if (pair? best-label-pos)
                      (car best-label-pos)
                      #f))))
        #f))

  (define (next)
    (let loop ((x (mcdr todo)))
      (if (mpair? x)
          (let* ((label-pos (mcar x))
                 (label (car label-pos)))
            (if (not (vector-ref dumped label))
                label
                (loop (mcdr x))))
          #f)))

  (define (schedule! label tail?)
    (let ((label-pos (cons label pos)))
      (if tail?
          (let ((cell (mcons label-pos '())))
            (set-mcdr! (mcar todo) cell)
            (set-mcar! todo cell))
          (let ((cell (mcons label-pos (mcdr todo))))
            (set-mcdr! todo cell)
            (when (eq? (mcar todo) todo)
              (set-mcar! todo cell))))))

  (define (dump)
    (let loop ((fallthrough-to-next? #t))
      (let ((label (get fallthrough-to-next?)))
        (when label
          (if (not (vector-ref dumped label))
              (begin
                (vector-set! dumped label #t)
                (loop (dump-bb label)))
              (loop fallthrough-to-next?))))))

  (define (dump-bb label)
    (let* ((bb (vector-ref bbs label))
           (rev-instrs (bb-rev-instrs bb))
           (jump (car rev-instrs))
           (opcode (car jump)))
      (emit label)
      (for-each
       (lambda (instr)
         (case (car instr)
           ((closure call-toplevel)
            (schedule! (cadr instr) #t)))
         (emit instr))
       (reverse (cdr rev-instrs)))
      (cond ((eq? opcode 'goto)
             (schedule! (cadr jump) #f)
             (if (not (equal? (cadr jump) (next)))
                 (begin
                   (emit jump)
                   #f)
                 #t))
            ((eq? opcode 'goto-if-false)
             (schedule! (cadr jump) #f)
             (schedule! (caddr jump) #f)
             (cond ((equal? (caddr jump) (next))
                    (emit (list 'goto-if-false (cadr jump)))
                    #t)
                   ((equal? (cadr jump) (next))
                    (emit (list 'prim '#%not))
                    (emit (list 'goto-if-false (caddr jump)))
                    #t)
                   (else
                    (emit (list 'goto-if-false (cadr jump)))
                    (emit (list 'goto (caddr jump)))
                    #f)))
            (else
             (case (car jump)
               ((jump-toplevel)
                (schedule! (cadr jump) #f)
                ;; it is not correct to remove jump-toplevel when label is next
                (if #t ;; (not (equal? (cadr jump) (next)))
                    (begin
                      (emit jump)
                      #f)
                    #t))
               (else
                (emit jump)
                #f))))))

  (set-mcar! todo todo) ;; make fifo

  (schedule! 0 #f)

  (dump)

  (reverse rev-code))
