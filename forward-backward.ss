#lang scheme/unit

(require
 (planet schematics/numeric:1/vector)
 "sigs.ss"
 "util.ss")

(import
 hmm^)
(export
 forward-backward^)

;; Forward/backward recursion taken from
;; "Smoothing Algorithms for State-Space Models"
;;   http://www.gatsby.ucl.ac.uk/~byron/nlds/briers04.pdf
;; Section III A. and B.

(define (smooth hmm data)
  (define fwd (forward hmm data))
  (vector-reverse (backward hmm data fwd)))

;; (Vectorof (Vectorof [0,1])) -> (Vectorof Natural)
(define (sample p)
  (for/vector ([i (vector-length p)]
               [b (in-vector p)])
    (multinomial-sample b)))

;; HMM (Vectorof Obs) -> (Vectorof [0,1])
;;
;; Calculate the normalised (alpha) values Pr(x_t | y_{1:t})
(define (forward hmm data)
  (define n-data (vector-length data))
  ;; Compute the forward probabilities
  ;; Pr(x_t | y_{1:t}), forall t
  ;;
  ;; (Vectorof (Vectorof [0,1]))
  (define-values (_ fwd)
    (for/fold/vector ([p (hmm-initial-probabilities hmm)])
        ([i n-data]
         [o (in-vector data)])
      (let* ([p-o (vector* p (hmm-observation-probabilities hmm o))]
             [p-a (vector/s p-o (vector-sum p-o))]
             [next-p
              (for/fold ([next-p (make-vector (vector-length p-a) 0)])
                  ([p-s (in-vector p-a)]
                   [s   (in-naturals)])
                (if (zero? p-s)
                    next-p
                    (vector+
                     next-p
                     (vector*s (hmm-transition-probabilities hmm s) p-s))))])
        (values next-p p-a))))
  fwd)

(define (backward hmm data fwd)
  (define n-data (vector-length data))
  ;; Compute the backward probabilities
  ;; Pr(x_t | y_{1:T}), for all t
  ;;
  ;; (Vectorof (Vectorof [0,1]))
  ;; Note values ordered from t=T to t=0
  (define-values (_ bwd)
    (for/fold/vector
      ;; Pr(x_{t+1} | y_{1:T})
     ([p (vector-ref fwd (sub1 n-data))])
     ([_ n-data]
      ;; This index into f + 1
      [i (in-range (sub1 n-data) -1 -1)])
     ;; Pr(x_t | y_{1:t})
     (if (zero? i)
         (values p p)
         (let* ([f (vector-ref fwd (sub1 i))]
                [n-states (vector-length f)]
                [normalisation
                 (for/fold ([z (make-vector n-states)])
                      ([s (in-range n-states)])
                    (vector+ z
                             (vector*s (hmm-transition-probabilities hmm s)
                                       (vector-ref f s))))]
                [next-p
                 (vector*
                   f
                   (for/vector ([s n-states])
                               (vector-sum
                                (vector/ (vector* p (hmm-transition-probabilities hmm s))
                                          normalisation))))])
           (values (vector/s next-p (vector-sum next-p)) p)))))
  bwd)