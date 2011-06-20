#lang racket

(provide tree-shake!)

(require "context.rkt")

(define (tree-shake! bbs)
  (tighten-jump-cascades! bbs)
  (let ((bbs (remove-useless-bbs! bbs)))
    (reorder! bbs)))

(define (bbs->ref-counts bbs)
  (let ((ref-counts (make-vector (vector-length bbs) 0)))

    (define visit
      (lambda (label)
        (let ((ref-count (vector-ref ref-counts label)))
          (vector-set! ref-counts label (+ ref-count 1))
          (when (= ref-count 0)
            (let* ((bb (vector-ref bbs label))
                   (rev-instrs (bb-rev-instrs bb)))
              (for-each
               (lambda (instr)
                 (let ((opcode (car instr)))
                   (cond ((eq? opcode 'goto)
                          (visit (cadr instr)))
                         ((eq? opcode 'goto-if-false)
                          (visit (cadr instr))
                          (visit (caddr instr)))
                         ((or (eq? opcode 'closure)
                              (eq? opcode 'call-toplevel)
                              (eq? opcode 'jump-toplevel))
                          (visit (cadr instr))))))
               rev-instrs))))))

    (visit 0)

    ref-counts))

(define (tighten-jump-cascades! bbs)
  (let ((ref-counts (bbs->ref-counts bbs)))

    (define resolve
      (lambda (label)
        (let* ((bb (vector-ref bbs label))
               (rev-instrs (bb-rev-instrs bb)))
          (and (or (null? (cdr rev-instrs))
                   (= (vector-ref ref-counts label) 1))
               rev-instrs))))

    (let loop1 ()
      (let loop2 ((i 0)
                  (changed? #f))
        (if (< i (vector-length bbs))
            (if (> (vector-ref ref-counts i) 0)
                (let* ((bb (vector-ref bbs i))
                       (rev-instrs (bb-rev-instrs bb))
                       (jump (car rev-instrs))
                       (opcode (car jump)))
                  (cond ((eq? opcode 'goto)
                         (let* ((label (cadr jump))
                                (jump-replacement (resolve label)))
                           (if jump-replacement
                               (begin
                                 (vector-set!
                                  bbs
                                  i
                                  (make-bb (bb-label bb)
                                           (append jump-replacement
                                                   (cdr rev-instrs))))
                                 (loop2 (+ i 1)
                                        #t))
                               (loop2 (+ i 1)
                                      changed?))))
                        ((eq? opcode 'goto-if-false)
                         (let* ((label-then (cadr jump))
                                (label-else (caddr jump))
                                (jump-then-replacement (resolve label-then))
                                (jump-else-replacement (resolve label-else)))
                           (if (and jump-then-replacement
                                    (null? (cdr jump-then-replacement))
                                    jump-else-replacement
                                    (null? (cdr jump-else-replacement))
                                    (or (eq? (caar jump-then-replacement)
                                             'goto)
                                        (eq? (caar jump-else-replacement)
                                             'goto)))
                               (begin
                                 (vector-set!
                                  bbs
                                  i
                                  (make-bb
                                   (bb-label bb)
                                   (cons
                                    (list
                                     'goto-if-false
                                     (if (eq? (caar jump-then-replacement)
                                              'goto)
                                         (cadar jump-then-replacement)
                                         label-then)
                                     (if (eq? (caar jump-else-replacement)
                                              'goto)
                                         (cadar jump-else-replacement)
                                         label-else))
                                    (cdr rev-instrs))))
                                 (loop2 (+ i 1)
                                        #t))
                               (loop2 (+ i 1)
                                      changed?))))
                        (else
                         (loop2 (+ i 1)
                                changed?))))
                (loop2 (+ i 1)
                       changed?))
            (when changed?
              (loop1)))))))

(define (remove-useless-bbs! bbs)
  (let ((ref-counts (bbs->ref-counts bbs)))
    (let loop1 ((label 0) (new-label 0))
      (if (< label (vector-length bbs))
          (if (> (vector-ref ref-counts label) 0)
              (let ((bb (vector-ref bbs label)))
                (vector-set!
                 bbs
                 label
                 (make-bb new-label (bb-rev-instrs bb)))
                (loop1 (+ label 1) (+ new-label 1)))
              (loop1 (+ label 1) new-label))
          (renumber-labels bbs ref-counts new-label)))))

(define (renumber-labels bbs ref-counts n)
  (let ((new-bbs (make-vector n)))
    (let loop2 ((label 0))
      (if (< label (vector-length bbs))
          (if (> (vector-ref ref-counts label) 0)
              (let* ((bb (vector-ref bbs label))
                     (new-label (bb-label bb))
                     (rev-instrs (bb-rev-instrs bb)))

                (define fix
                  (lambda (instr)

                    (define new-label
                      (lambda (label)
                        (bb-label (vector-ref bbs label))))

                    (let ((opcode (car instr)))
                      (cond ((eq? opcode 'closure)
                             (list 'closure
                                   (new-label (cadr instr))))
                            ((eq? opcode 'call-toplevel)
                             (list 'call-toplevel
                                   (new-label (cadr instr))))
                            ((eq? opcode 'jump-toplevel)
                             (list 'jump-toplevel
                                   (new-label (cadr instr))))
                            ((eq? opcode 'goto)
                             (list 'goto
                                   (new-label (cadr instr))))
                            ((eq? opcode 'goto-if-false)
                             (list 'goto-if-false
                                   (new-label (cadr instr))
                                   (new-label (caddr instr))))
                            (else
                             instr)))))

                (vector-set!
                 new-bbs
                 new-label
                 (make-bb new-label (map fix rev-instrs)))
                (loop2 (+ label 1)))
              (loop2 (+ label 1)))
          new-bbs))))

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
