#lang scheme/base

(require
 scheme/unit)

(define-signature node-config^
  ;; type P : The parameters (typically, prior) for a node implementation
  (p))

(define-signature node^
  ;; type P :   The parameters for this node implementation
  ;; 
  ;; type Obs : The observations for this node implementation
  (;; -> Node
   create-node
   ;; Node -> Natural
   node-n
   ;; Node -> P
   node-params
   ;; Obs -> [0,1]
   node-prior-likelihood
   ;; Node Obs -> [0,1]
   node-likelihood
   ;; Node -> Node
   node-add
   ;; Node -> Node
   node-remove))

(define-signature bp-config^
  (;; Number > 0, the concentration parameter
   c
   ;; Number > 0, the mass of B_0
   gamma))

(define-signature bp^
  ;; type Node
  (;; -> BP
   create-bp
   ;; BP -> (values (Vectorof {0,1}) Natural)
   bp-sample-transitions
   ;; BP -> (values (Vectorof [0,1]) (Vectorof [0,1]))
   bp-sample-weights
   ;; BP (Vectorof Obs) (Vectorof Natural) -> BP
   bp-update
   ;; BP Natural -> Node
   bp-node-ref
   ;; BP Natural -> [0,1]
   bp-transition-likelihood
   ;; BP Natural -> [0,1]
   bp-weight-expectation
   ;; BP -> Natural
   bp-n-visited))

(define-signature hmm^
  (;; -> HMM
   create-hmm
   ;; HMM -> (Vectorof [0,1])
   hmm-initial-probabilities
   ;; HMM Natural -> (Vectorof [0,1])
   hmm-transition-probabilities
   ;; HMM (Vectorof {0,1}) Obs -> (Vectorof [0,1])
   hmm-observation-probabilities
   ;; HMM (Vectorof Obs) (Vectorof Natural) -> HMM
   hmm-update))

(define-signature forward-backward^
  (;; HMM (Vectorof Obs) -> (Vectorof (Vectorof [0,1]))
   smooth
   ;; HMM (Vectorof Obs) -> (Vectorof Natural)
   sample
   ;; HMM (Vectorof Obs) -> (Vectorof (Vectorof [0,1]))
   forward
   ;; HMM (Vectorof Obs) (Vectorof (Vectorof [0,1])) -> (Vectorof [0,1])
   backward))

(provide
 node-config^
 node^
 bp-config^
 bp^
 hmm^
 forward-backward^)