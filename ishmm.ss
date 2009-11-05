#lang scheme/unit

(require
 scheme/match
 "base.ss"
 "sigs.ss")

(import
 node^
 bp^)

(export
 hmm^)

;; type Probabilities : (Vectorof [0,1]))

;; struct ishmm : BP Natural (U #f Probabilities) (Vectorof (U #f Probabilities))
;; 
;; offset -- The index the next bunch of unvisited nodes start at
;;
;; initial -- Initial state distribution
;;
;; transitions -- Transition probabilities. I.e. w*e / sum(w*e)
;;
;; The structure is mutable but appears immutable to the
;; outside. That is, every function call will always return
;; the same answer -- it is just that we may not have
;; computed the answer at the time of the call.
(define-struct ishmm (bp offset transitions) #:mutable)


;; -> ISHMM
(define (create-ishmm)
  (make-ishmm (make-bp) 0 #f (vector)))

;; ISHMM -> (Vectorof [0,1])
(define (hmm-initial-probabilities hmm)
  (match-define (struct ishmm [bp o i t]) hmm)
  (if i
      i
      (let ([ps (sample-transitions bp offset)])
        (set-ishmm-initial! hmm ps)
        ps)))

;; ISHMM Obs -> (Vectorof [0,1])
(define (hmm-observation-probabilities hmm obs)
  (match-define (struct ishmm [bp o i t]) hmm)
  (for/vector ([i o])
    (node-likelihood (bp-node-ref bp i) obs)))

;; ISHMM Natural -> (Vectorof [0,1])
(define (hmm-transition-probabilities hmm node-idx)
  (match-define (struct ishmm [bp o i t]) hmm)
  (if (< node-idx (vector-length t))
      (let ([ps (vector-ref t node-idx)])
        (if ps
            ps
            (let ([ps (sample-transitions bp o)])
              (vector-set! t node-idx ps)
              ps)))
      (let ([ps (sample-transitions bp o)])
        (set-ishmm-transitions! hmm (transitions-extend t ps node-idx))
        ps)))


;; Internal functions

(define (sample-transitions bp offset)
  (let-values* (([ts uv-ts] (bp-sample-transitions bp))
                ([ws uv-ws] (bp-sample-weights bp (vector-length uv-ts))))
    (weights+transitions->probabilities ws uv-ws ts uv-ts offset)))

(define (weights+transitions->probabilities ws uv-ws ts uv-ts offset)
  (define total-weight (+ (vector-sum ws) (vector-sum uv-ws)))
  (define n-visited (vector-length ws))
  (define n-unvisited (vector-length uv-ws))
  (define n (+ (vector-length ws) offset (vector-length uv-ws)))

  (for/vector ([i n])
    (cond
     [(< i n-visited)
      (/ (* (vector-ref ts i) (vector-ref ws i)) total-weight)]
     [(and (<= n-visited i) (< i (+ n-visited offset)))
      0.0]
     [else
      (/ (* (vector-ref uv-ts (- i offset)) (vector-ref ws (- i offset)))
         total-weight)])))


(define (transitions-extend t ps node-idx)
  (define n-existing (vector-length t))
  (if (< node-idx n-existing)
      (raise-mismatch-error 'transitions-extend
                            "Given node-idx that is in existing transitions"
                            node-idx)
      (for/vector ([i node-idx])
        (cond
         [(< i n-existing)
          (vector-ref t i)]
         [(and (<= n-existing i) (< i node-idx))
          #f]
         [else
          ps]))))