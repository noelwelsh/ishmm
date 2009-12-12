#lang scheme/base

(require 
 scheme/unit
 (planet williams/science:3/statistics)
 (planet schematics/numeric:1/vector)
 "trace.ss"
 "sample.ss"
 "../base.ss"
 "../sigs.ss"
 "../forward-backward.ss")


(define burn-in 0)
(define iterations 5000)
(define sample-interval 1)

(define true-ll null)
(define last-a null)
(define last-assignments null)
(define last-ll null)
(define last-l1-distance null)

(define (experiment name file-name trace hmm@ node@)
  (define-values/invoke-unit/infer (export forward-backward^ hmm^)
    (link hmm@ node@ forward-backward@))
  (define data (trace->data trace))
  (define true-assignments (trace->assignments trace))
  (define-values (assignments ll a)
    (iterate data iterations
                  #:burn-in burn-in
                  #:sample-interval sample-interval))
  (set! true-ll
        (sample-log-likelihood
         (create-specific-sample data (lambda (d) true-assignments))))
  (set! last-a a)
  (set! last-assignments assignments)
  (set! last-ll ll)
  (set! last-l1-distance
        (map (lambda (a-m)
               (adjacency-matrix-l1-distance true-a-m a-m))
             (map assignments->adjacency-matrix assignments)))

  (with-output-to-file file-name
    (lambda ()
      (display true-ll)(newline)
      (display data)(newline)
      (display last-a)(newline)
      (display last-ll)(newline)
      (display last-l1-distance)(newline)
      (display last-assignments)(newline))
    #:exists 'replace)
  )  

;;; Utilities

;; (Vectorof Obs) [Natural] [Pseudo-Random-Generator] -> (Vectorof Natural)
(define (random-assignments data [n-groups 10] [generator (current-pseudo-random-generator)])
  (for/vector ([i (vector-length data)])
    (random n-groups generator)))

(provide (all-defined-out))
