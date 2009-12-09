#lang scheme/base

;; Base exn type
(define-struct (exn:ishmm exn) ())

;; Exn raised when there are no transitions with non-zero probability
(define-struct (exn:ishmm:no-transitions exn:ishmm) ())

(provide
 (struct-out exn:ishmm)
 (struct-out exn:ishmm:no-transitions))