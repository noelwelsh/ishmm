#lang scheme/unit

(require
 "../sigs.ss")

(import
 hmm^
 forward-backward^)

(export
 sample^)

(define (iterate data n-iterations initial-assignment
                 #:burn-in [burn-in 0]
                 #:sample-interval [sample-interval 1000])
  (define iterations-per-sample (floor (/ n-iterations sample-interval)))
  (define-values (hmm assignment)
    (for/fold ([hmm (hmm-update (create-hmm) data initial-assignment)])
        ([i (in-range burn-in)])
      (let ([assignment (sample (smooth hmm data))])
        (values assignment (hmm-update hmm data assignment)))))
  
  (for/fold ([assignments (list assigment)]
             [ll (list (sample-log-likelihood sample))]
             [a  (list (sample-a sample))])
            ([s (in-range iterations-per-sample)])
    (for ([i (in-range sample-interval)])
         (gibbs-iterate! sample))
    (format "Sample ~a: " s) (display (sample-statistics sample))(newline)
    (values (cons (vector-copy (sample-assignments sample)) assignments)
            (cons (sample-log-likelihood sample) ll)
            (cons (sample-a sample) a))))

(provide
 iterate)