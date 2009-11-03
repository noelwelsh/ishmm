#lang scheme/unit

(require
 "base.ss"
 "sigs.ss")

(import
 node^
 bp^)

(export
 ishmm^)

;; type Weights     : (Vectorof [0,1])
;; type Transitions : (Vectorof {0,1})

;; struct ishmm : BP (Vector Weights) (Vectorof Transitions)
(define-struct ishmm (bp weights transitions))

;; -> ISHMM
(define (create-ishmm)
  (make-ishmm (make-bp) (vector) (vector)))


;; ISHMM (Vectorof Obs) -> ISHMM
(define (ishmm-iterate ishmm data)
  (define n-data (vector-length data))
  ;; Compute the forward probabilities
  ;; Pr(x_t | y_{1:t}), forall t
  ;;
  ;; (Vectorof (Vectorof [0,1]))
  (define-values (_ forward)
    ;; We start in state 0
    (for/fold/vector ([p (vector 1)])
        ([i n]
         [o (in-vector obs)])
      (let* ([p-o (vector* p (observation-probabilities ishmm p o))])
        (for/fold ([p-t (make-evector)])
            ([p-i (in-vector p-o)]
             [i   (in-naturals)])
          (if (zero? p-i)
              p-t ;; Do nothing
              (evector+
               p-t
               (vector*s (transition-probabilities ishmm i) p-i)))))))
  ;; Compute the backward probabilities
  ;; Pr(x_t | y_{1:T}), for all t
  ;;
  ;; (Vectorof (Vectorof [0,1]))
  (define backward
    (for/fold/vector
     ;; Pr(x_{t+1} | y_{1:t})
     ([p (vector-ref forward (sub1 n-data))])
     ([i n]
      ;; Pr(x_t | y_{1:t})
      [a  (in-vector forward (- n-data 2) 0 -1)])

     (values
      ;; Becomes Pr(x_{t+1} | y_{1:t})
      a
      ;; Pr(x_t | y_{1:T})
      (vector* a
               (/ (vector* p (transition-probabilities ishmm ...))
                  (vector-sum p))))))
  ;; Sample from the smoothed probabilities
  )


;;
;; Internal functions
;;

;; ISHMM (Vectorof [0,1]) Obs -> (Vectorof [0,1])
(define (observation-probabilities ishmm p o)
  )

;; ISHMM Natural -> (Vectorof [0,1])
(define (transition-probabilities ishmm node-idx)
  )