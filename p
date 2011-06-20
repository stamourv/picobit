#!/usr/bin/env racket
#lang racket

(define (usage)
  (printf "usage: p path/to/file.scm\n")
  (exit 1))

(if (= (vector-length (current-command-line-arguments)) 1)
    (let ([file (vector-ref (current-command-line-arguments) 0)])
      (void (system* "picobit"    (path-replace-suffix file ".scm"))
            (system* "picobit-vm" (path-replace-suffix file ".hex"))))
    (usage))
