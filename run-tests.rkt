#lang racket

(require rackunit rackunit/text-ui)

;; This tests whether the programs produce the expected output.
;; This is a pretty weak equivalence, and doesn't catch optimization
;; regressions. Checking for bytecode equality would be too strong an
;; equivalence.
;; This should be fixed.

(define (test-file? file)
  (and (regexp-match? #rx"[.]scm$" file)
       ;; skip emacs temp unsaved file backups
       (not (regexp-match "^\\.#" file))))

(define (run-one file f)
  (when (test-file? file)
    (let* ([file-str (path->string file)]
           [hex (path-replace-suffix file ".hex")]
           [expected (path-replace-suffix file ".expected")]
           [input    (path-replace-suffix file ".input")])
      (test-suite
       file-str
       (begin (test-case "no expected file"
                         (check-true (file-exists? expected)))
              (when (file-exists? expected)
                (f file-str hex expected input))
              (when (file-exists? hex)
                (delete-file hex)))))))

(define (run-succeed file)
  (run-one
   file
   (lambda (file-str hex expected input)
     (system* "./picobit" file-str)
     (test-case "compilation" (check-true (file-exists? hex)))
     (when (file-exists? hex)
       (let ([out (with-output-to-string
                    (lambda ()
                      (parameterize
                          ([current-input-port
                            (if (file-exists? input)
                                (open-input-file input)
                                (open-input-string ""))])
                        (system* "./picobit-vm" hex))))])
         (test-case "execution"
                    (check-equal? out (file->string expected))))))))

(define (run-fail-compile file)
  (run-one
   file
   (lambda (file-str hex expected input)
     (let ([out (with-output-to-string
                  (lambda ()
                    (system* "./picobit" file-str)))])
       (test-case "compilation error"
                  (check-false (file-exists? hex))
                  (check-equal? out (file->string expected)))))))

(define (run-fail-execute file) (run-succeed file))

(define (run-single file)
  (let*-values ([(path p b) (split-path file)]
                [(dir)      (path->string path)])
    (cond [(equal? dir "tests/succeed/")
           (run-succeed file)]
          [(equal? dir "tests/fail/compile/")
           (run-fail-compile file)]
          [(equal? dir "tests/fail/execute/")
           (run-fail-execute file)])))

(define args (current-command-line-arguments))

(void
 (run-tests
  (cond [(>= (vector-length args) 1) ; run one
         (run-single (string->path (vector-ref args 0)))]
        [else ; run all
         (make-test-suite
          "PICOBIT tests"
          (filter (lambda (x) (not (void? x)))
                  (append
                   (for/list ([file (in-directory "./tests/succeed/")])
                     (run-succeed file))
                   (for/list ([file (in-directory "./tests/fail/compile/")])
                     (run-fail-compile file))
                   (for/list ([file (in-directory "./tests/fail/execute/")])
                     (run-fail-execute file)))))])))
