#lang scheme/base

(require
 scheme/unit)

(define-signature node^
  ;; type P :   The parameters for this node implementation
  ;; 
  ;; type Obs : The observations for this node implementation
  (;; -> Node
   create-node
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
  (;; Number > 0
   c))

(define-signature bp^
  ;; type Node
  (;; -> BP
   create-bp
   ;; BP -> ???
   bp-sample-transitions
   ;; BP -> ???
   bp-sample-weights
   ;; BP ??? -> BP
   bp-update
   ;; BP Natural -> Node
   bp-node-ref))

(define-signature ishmm^
  (;; -> ISHMM
   create-ishmm
   ;; ISHMM (Vectorof Obs) -> ISHMM
   ishmm-iterate))

(provide
 node^
 bp-config^
 bp^
 ishmm^)