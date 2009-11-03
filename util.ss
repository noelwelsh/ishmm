#lang scheme/base

;; The amount of roundoff error allowed in a vector that should sum to one
(define e 0.00001)
(define one (+ 1 e))

;; ((vectorof [0,1]) [random-generator] -> (U integer #f))
;;
;; Sample from a sub-normalised multinomial distribution.
;; The vector v consists of number in [0,1] that sum to <=
;; one.  Each element indicates the probability of selecting
;; that index.  The result is the selected index, or #f if
;; no index is selected (which can occur if the
;; probabilities sum to less than one).
(define (multinomial-sample v [generator (current-pseudo-random-generator)])
  (define p (random generator))
  (define-values (sum choice)
    (for/fold ([sum 0] [choice #f])
              ([x (in-vector v)] [i (in-naturals)])
      (if (zero? x)
          (values sum choice)
          (let ([new-sum (+ sum x)])
            (if (> new-sum one)
                (raise-type-error 'multinomial-sample "vector summing to one" v)
                (if (and (<= sum p) (< p new-sum))
                    (values new-sum i)
                    (values new-sum choice)))))))
  ;;(printf "multinomial-sample: ~a  ~a\n" p choice)
  choice)
  

(provide multinomial-sample)