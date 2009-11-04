#lang scheme/unit

(require
 "base.ss"
 "sigs.ss")

(import
 node^
 bp^)

(export
 hmm^)

;; type Weights     : (Vectorof (Vectorof [0,1]))
;; type Transitions : (Vectorof (Vectorof {0,1}))

;; struct ishmm : BP (Vector Weights) (Vectorof Transitions)
(define-struct ishmm (bp weights transitions))

;; -> ISHMM
(define (create-ishmm)
  (make-ishmm (make-bp) (vector) (vector)))

;; ISHMM -> (Vectorof [0,1])
(define (hmm-initial-probabilities ishmm)
  )

;; ISHMM Obs -> (Vectorof [0,1])
(define (hmm-observation-probabilities ishmm o)
  )

;; ISHMM Natural -> (Vectorof [0,1])
(define (hmm-transition-probabilities ishmm node-idx)
  )