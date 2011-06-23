#lang racket

(require rackunit rackunit/text-ui)

;; This tests whether the programs produce the expected output.
;; This is a pretty weak equivalence, and doesn't catch optimization
;; regressions. Checking for bytecode equality would be too strong an
;; equivalence.
;; This should be fixed.

(define (run-one file)
  (let* ([file-str (path->string file)]
         [hex (path-replace-suffix file ".hex")]
         [expected (path-replace-suffix file ".expected")])
    (test-suite
     file-str
     (begin
       (test-case "no expected file" (check-true (file-exists? expected)))
       (when (file-exists? expected)
         (system* "./picobit" file-str)
         (test-case "compilation" (check-true (file-exists? hex)))
         (when (file-exists? hex)
           (let ([out (with-output-to-string
                        (lambda ()
                          (system* "./picobit-vm" hex)))])
             (test-case "execution"
                        (check-equal? out (file->string expected))))
           (delete-file hex)))))))

(define args (current-command-line-arguments))

(void
 (run-tests
  (cond [(>= (vector-length args) 1) ; run one
         (run-one (string->path (vector-ref args 0)))]
        [else ; run all
         (make-test-suite
          "PICOBIT tests"
          (for/list ([file (in-directory "./tests/")]
                     #:when (and (regexp-match? #rx"[.]scm$" file)
                                 ;; skip emacs temp unsaved file backups
                                 (not (regexp-match "^\\.#" file))))
            (run-one file)))])))
