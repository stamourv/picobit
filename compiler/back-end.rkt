#lang racket

(require "ir.rkt" "ast.rkt")

;; Back-end utilities.

;-----------------------------------------------------------------------------

(provide renumber-labels)

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

;-----------------------------------------------------------------------------

(provide code->vector)

(define (code->vector code)
  (let ((v (make-vector (+ (code-last-label code) 1))))
    (for-each
     (lambda (bb)
       (vector-set! v (bb-label bb) bb))
     (code-rev-bbs code))
    v))

;-----------------------------------------------------------------------------

(provide resolve-toplevel-labels!)

(define (resolve-toplevel-labels! bbs)
  (for ([i (in-range (vector-length bbs))])
    (let* ([bb (vector-ref bbs i)]
           [rev-instrs (bb-rev-instrs bb)])
      (set-bb-rev-instrs!
       bb
       (map (match-lambda
              [`(,(and opcode (or 'call-toplevel 'jump-toplevel)) ,arg)
               `(,opcode ,(prc-entry-label arg))]
              [instr
               instr])
            rev-instrs)))))
