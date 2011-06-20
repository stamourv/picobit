#lang racket

(provide tree-shake!)

(require "context.rkt" "back-end.rkt")

(define (tree-shake! bbs)
  (tighten-jump-cascades! bbs)
  (remove-useless-bbs! bbs))

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
