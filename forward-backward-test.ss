#lang scheme/base

(require
 scheme/unit
 (planet schematics/schemeunit:3/test)
 (planet schematics/numeric:1/vector)
 "sigs.ss"
 "forward-backward.ss")

(define e 0.0001)

;; An HMM implementation for testing purposes
;;
;; Taken from
;;
;;  http://en.wikipedia.org/wiki/Forward-backward_algorithm
;;
(define-unit test-hmm@
  (import)
  (export hmm^)

  (define i
    (vector 0.5 0.5))
  
  (define t
    (vector
     (vector 0.7 0.3)
     (vector 0.3 0.7)))

  ;; Indexed by observation first, then state
  (define o
    (vector
     (vector 0.9 0.2)
     (vector 0.1 0.8)))
  
  (define (create-hmm)
    #f)

  (define (hmm-initial-probabilities hmm)
    i)
  
  (define (hmm-transition-probabilities hmm idx)
    (vector-ref t idx))

  (define (hmm-observation-probabilities hmm obs)
    (vector-ref o obs))
  )

(define-values/invoke-unit/infer test-hmm@)
(define-values/invoke-unit/infer forward-backward@)

;; Data and results from the 'pedia of Wiki as ref'ed above
(define data #(0 0 1 0 0))
(define fwd
  (vector (vector .8182 .1818)
          (vector .8834 .1166)
          (vector .1907 .8093)
          (vector .7308 .2692)
          (vector .8673 .1327)))

(define bwd
  (vector (vector .6273 .3727)
          (vector .6533 .3467)
          (vector .3763 .6237)
          (vector .5923 .4077)
          (vector .6569 .3531)))

(printf "~a\n" (backward #f data fwd))

(define/provide-test-suite forward-backward-tests
  (test-case
   "forward probabilities correct"
   (vector-map
    (lambda (v1 v2) (check-vector= v1 v2 e))
    (forward (create-hmm) data)
    fwd))

  (test-case
   "backward probabilities correct"
   (vector-map
    (lambda (v1 v2) (check-vector= v1 v2 e))
    (backward (create-hmm) data fwd)
    bwd))
  
  (test-case
   "Smoothed probabilities calculated correctly"
   (vector-map
    (lambda (v1 v2) (check-vector= v1 v2 e))
    (smooth (create-hmm) #(0 1))
    #(#(0 0 0) #(0 0 0))))
  )