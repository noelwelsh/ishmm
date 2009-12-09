#lang scheme/unit

(require
 scheme/match
 (planet schematics/numeric:1/vector)
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
(define-struct ishmm (bp offset initial transitions) #:mutable #:transparent)


;; -> ISHMM
(define (create-hmm)
  (make-ishmm (create-bp) 0 #f (vector)))

;; ISHMM -> (Vectorof [0,1])
(define (hmm-initial-probabilities hmm)
  (match-define (struct ishmm [bp o i t]) hmm)
  (if i
      i
      (let ([ps (sample-transitions bp o)])
        (if (zero? (vector-sum ps))
          (raise (make-exn:ishmm:no-transitions
                  "iSHMM initial transition probabilities all zero"
                  (current-continuation-marks)))
          (begin
            (set-ishmm-initial! hmm ps)
            ps)))))

;; ISHMM Obs -> (Vectorof [0,1])
(define (hmm-observation-probabilities hmm obs)
  (match-define (struct ishmm [bp o i t]) hmm)
  ;;(printf "hmm-observation-probabilities offset ~a\n" o)
  (for/vector ([i o])
    (node-likelihood (bp-node-ref bp i) obs)))

;; ISHMM Natural -> (Vectorof [0,1])
(define (hmm-transition-probabilities hmm node-idx)
  (match-define (struct ishmm [bp o i t]) hmm)
  ;;(printf "~a ~a\n" node-idx t)
  (if (< node-idx (vector-length t))
      (let ([ps (vector-ref t node-idx)])
        (if ps
            ps
            (let ([ps (sample-transitions bp o)])
              (vector-set! t node-idx ps)
              ps)))
      (let* ([ps (sample-transitions bp o)]
             [new-t (transitions-extend t ps node-idx)]
             [new-offset (vector-length ps)])
        ;;(printf "hmm-transition-probabilities new offset ~a\n" (vector-length ps))
        (set-ishmm-transitions! hmm new-t)
        (set-ishmm-offset! hmm new-offset)
        ps)))

;; ISHMM (Vectorof Obs) (Vectorof Natural) -> ISHMM
(define (hmm-update hmm data assignments)
  (match-define (struct ishmm [bp o i t]) hmm)
  (define new-bp (bp-update bp data assignments))
  ;;(printf "hmm-update new offset ~a\n" (bp-n-visited new-bp))
  (make-ishmm new-bp (bp-n-visited new-bp) #f (vector)))


;;; Internal functions

(define (sample-transitions bp offset)
  (let*-values (([ts uv-ts] (bp-sample-transitions bp))
                ([ws uv-ws] (bp-sample-weights bp uv-ts)))
    (weights+transitions->probabilities ws uv-ws ts uv-ts offset)))

(define (weights+transitions->probabilities ws uv-ws ts uv-ts offset)
  (define n-visited (vector-length ts))
  (define n-unvisited uv-ts)
  (define n (+ n-visited (- offset n-visited) n-unvisited))
  (define ps
    (for/vector ([i n])
      (cond
       [(< i n-visited)
        (* (vector-ref ts i) (vector-ref ws i))]
       [(and (<= n-visited i) (< i offset))
        0.0]
       [else
        (vector-ref uv-ws (- i offset))])))
  (define total-weight (vector-sum ps))
  
  ;(when (zero? total-weight)
  ;  (raise (make-exn:ishmm:no-transitions
  ;          "iSHMM sampled no transitions"
  ;          (current-continuation-marks))))

  (if (zero? total-weight)
      ps
      (vector/s ps total-weight)))


(define (transitions-extend t ps node-idx)
  (define n-existing (vector-length t))
  (when (< node-idx n-existing)
    (raise-mismatch-error 'transitions-extend
                          "Given node-idx that is in existing transitions"
                          node-idx))
  
  (for/vector ([i (add1 node-idx)])
    (cond
     [(< i n-existing)
      (vector-ref t i)]
     [(and (<= n-existing i) (< i node-idx))
      #f]
     [else
      ps])))