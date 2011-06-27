#lang racket

(provide tree-shake!)

(require "ir.rkt" "back-end.rkt")

(define (tree-shake! bbs)
  (tighten-jump-cascades! bbs)
  (remove-useless-bbs! bbs))

(define (bbs->ref-counts bbs)
  (let ([ref-counts (make-vector (vector-length bbs) 0)])

    (define (visit label)
      (let ([ref-count (vector-ref ref-counts label)])
        (vector-set! ref-counts label (+ ref-count 1))
        (when (= ref-count 0)
          (for ([instr (in-list (bb-rev-instrs (vector-ref bbs label)))])
            (match instr
              [`(goto ,arg)
               (visit arg)]
              [`(goto-if-false ,a1 ,a2)
               (visit a1)
               (visit a2)]
              [`(,(or 'closure 'call-toplevel 'jump-toplevel) ,arg)
               (visit arg)]
              [_ (void)])))))

    (visit 0)

    ref-counts))

(define (tighten-jump-cascades! bbs)
  (let ([ref-counts (bbs->ref-counts bbs)])

    (define (resolve label)
      (let ([rev-instrs (bb-rev-instrs (vector-ref bbs label))])
        (and (or (null? (cdr rev-instrs))
                 (= (vector-ref ref-counts label) 1))
             rev-instrs)))

    (define (iterate)

      (define changed?
        (for/fold ([changed? #f])
            ([(cur-bb i) (in-indexed (vector-length bbs))]
             #:when (> (vector-ref ref-counts i) 0))
          (match cur-bb

            [(bb label `(,jump . ,rest))
             (match jump
               [`(goto ,label)
                (let ([jump-replacement (resolve label)])
                  (if jump-replacement
                      ;; void is non-false, counts as a change
                      (vector-set! bbs i
                                   (make-bb label
                                            (append jump-replacement rest)))
                      changed?))]

               [`(goto-if-false ,label-then ,label-else)
                (let* ([jump-then-replacement (resolve label-then)]
                       [jump-else-replacement (resolve label-else)]
                       [just-jump-then
                        (and jump-then-replacement
                             (null? (cdr jump-then-replacement)))]
                       [just-jump-else
                        (and jump-else-replacement
                             (null? (cdr jump-else-replacement)))]
                       [then-goto (eq? (caar jump-then-replacement) 'goto)]
                       [else-goto (eq? (caar jump-else-replacement) 'goto)])
                  (if (and just-jump-then just-jump-else
                           (or then-goto else-goto))
                      ;; void is non-false, counts as a change
                      (vector-set! bbs i
                                   (make-bb
                                    label
                                    `((goto-if-false
                                       ,(if then-goto
                                            (cadar jump-then-replacement)
                                            label-then)
                                       ,(if else-goto
                                            (cadar jump-else-replacement)
                                            label-else))
                                      . rest)))
                      changed?))])]
            [_ changed?])))

      (when changed?
        (iterate)))

    (iterate)))

(define (remove-useless-bbs! bbs)
  (define ref-counts (bbs->ref-counts bbs))
  (define new-label
    (for/fold ([new-label 0])
        ([(bb label) (in-indexed bbs)]
         [ref-count  (in-vector  ref-counts)]
         #:when (> ref-count 0))
      (vector-set! bbs label (make-bb new-label (bb-rev-instrs bb)))
      (+ new-label 1)))
  (renumber-labels bbs ref-counts new-label))
