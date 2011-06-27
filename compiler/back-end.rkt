#lang racket

(require "ir.rkt" "ast.rkt")

;; Back-end utilities.

;-----------------------------------------------------------------------------

(provide renumber-labels)

(define (renumber-labels bbs ref-counts n)

  (define (fix instr)
    (define (make-new-label label)
      (bb-label (vector-ref bbs label)))
    (match instr
      [`(,(and opcode (or 'closure 'call-toplevel 'jump-toplevel 'goto)) ,arg)
       (list opcode (make-new-label arg))]
      [`(goto-if-false ,a1 ,a2)
       (list 'goto-if-false (make-new-label a1) (make-new-label a2))]
      [_ instr]))

  (let ([new-bbs (make-vector n)])
    (for ([(b label) (in-indexed bbs)]
          #:when (> (vector-ref ref-counts label) 0))
      (match b
        [(bb new-label rev-instrs)
         (vector-set! new-bbs new-label
                      (make-bb new-label (map fix rev-instrs)))]))
    new-bbs))

;-----------------------------------------------------------------------------

(provide code->vector)

(define (code->vector code)
  (let ([v (make-vector (+ (code-last-label code) 1))])
    (for ([bb (in-list (code-rev-bbs code))])
      (vector-set! v (bb-label bb) bb))
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
