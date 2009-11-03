#lang scheme/unit

(require
 "base.ss"
 "sigs.ss")

(import
 node^
 bp-config^)

(export
 bp^)

;; struct bp : Number Number (Vectorof Number) (Vectorof Node)
;;
;; c is concentration parameter
;;
;; n is the number of posterior updates, and hence the
;; mixing proportion between the discrete and continuous
;; parts of the BP
;;
;; w is the vector of weights given to the nodes
;;
;; n is the already sampled nodes, discrete part of the BP.
(define-struct bp (c n w n))


;; -> BP
(define (create-bp)
  ...)

;; BP -> ???
(define (bp-sample-transitions bp)
  ...)

;; BP -> ???
(define (bp-sample-weights bp)
  ...)

;; BP ??? -> BP
(define (bp-update bp data)
  ...)

;; BP Natural -> Node
(define (bp-node-ref bp idx)
  ...)