#lang racket

(require rackunit rackunit/text-ui)

;; This tests for equality of _bytecode_ with the Gambit-based version
;; of picobit. Bytecode equality was a useful notion to ensure that
;; the Racket port was working correctly, but it's an overly
;; restrictive notion in general.
;; Since the Racket port is now complete, this test harness is not
;; deprecated.
;; A better test harness (and test suite) that checks the results of
;; programs instead of the programs themselves should be written.
(void
 (run-tests
  (make-test-suite
   "PICOBIT tests"
   (for/list ([file (in-directory "./tests/")]
              #:when (and (regexp-match? #rx"[.]scm$" file)
                          ;; skip emacs temp unsaved file backups
                          (not (regexp-match "^\\.#" file))))

     (let* ([file-str (path->string file)]
            [hex (path-replace-suffix file ".hex")]
            [expected (path-replace-suffix file ".expected")])
       (test-suite
        file-str
        (if (file-exists? expected)
            (dynamic-wind
              (λ () (system* "./picobit" file-str))
              (λ () (if (file-exists? hex)
                        (check-equal? (file->string hex)
                                      (file->string expected))
                        (printf "~a did not compile!\n" file-str)))
              (λ () (if (file-exists? hex)
                        (delete-file hex)
                        '())))
            (printf "File ~a has no expected file!\n" file-str))))))))
