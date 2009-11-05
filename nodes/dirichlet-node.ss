#lang scheme/unit

(require (planet schematics/numeric:1/vector)
         "../sigs.ss")

(import node-config^)
(export node^)

;; struct node : natural (vectorof number)
;;
;; n is the number of elements assigned to this node
;;
;; p is the parameters of the Dirichlet distribution this
;; node represents
(define-struct node (n p) #:transparent)

(define (create-node)
  (make-node 0 (vector-copy p)))

(define (node-params node)
  (node-p node))

;; (obs -> [0,1])
(define (node-prior-likelihood obs)
  (/ (vector-ref p obs)  (vector-sum p)))

;; (node obs -> [0,1])
;;
;; Assumes parameters are all integer values.  
(define (node-likelihood node obs)
  (define p (node-p node))
  (/ (vector-ref p obs)  (vector-sum p)))

(define (node-add node obs)
  (define new-p (vector-copy (node-p node)))
  (vector-add1! new-p obs)
  (make-node (add1 (node-n node)) new-p))

(define (node-remove node obs)
  (define new-p (vector-copy (node-p node)))
  (vector-sub1! new-p obs)
  (make-node (sub1 (node-n node)) new-p))


;; Miscellaneous

;; (node -> (U #t #f))
(define (node-empty? node)
  (zero? (node-n node)))

;; (node -> node)
(define (node-copy node)
  (make-node (node-n node) (vector-copy (node-p node))))