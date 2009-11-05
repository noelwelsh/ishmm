#lang scheme/unit

(require (planet schematics/numeric:1/vector)
         "../sigs.ss")

(import node-config^)
(export node^)

;; Implementation of nodes that are a vector of
;; dirichlets.  I.e. the likelihood function is a product of
;; Dirichlets.

;; struct node : natural (vectorof (vectorof number))
;;
;; n is the number of elements assigned to this node
;;
;; p is the parameters of the Dirichlet distributions this
;; node represents
(define-struct node (n p) #:transparent)

;; (obs -> node)
(define (create-node)
  (make-node 0 (copy-p p)))

(define (node-params node)
  (node-p node))

;; (obs -> [0,1])
(define (node-prior-likelihood obs)
  (for/fold ([p 1])
            ([p-part (in-vector p)]
             [i (in-vector obs)])
    (* p (/ (vector-ref p-part i)  (vector-sum p-part)))))

(define (node-add node obs)
  (define p (copy-p (node-p node)))
  (for ([p (in-vector p)]
        [i (in-vector obs)])
    (vector-add1! p i))
  (make-node (add1 (node-n node)) p))

(define (node-remove node obs)
  (define p (copy-p (node-p node)))
  (for ([p (in-vector p)]
        [i (in-vector obs)])
    (vector-sub1! p i))
  (make-node (sub1 (node-n node)) p))

;; (node natural -> [0,1])
;;
;; Assumes parameters are all integer values.  
(define (node-likelihood node obs)
  (for/fold ([pr 1])
            ([p  (in-vector (node-p node))]
             [i  (in-vector obs)])
    (* pr (/ (vector-ref p i)  (vector-sum p)))))


;;; Miscellaneous functions

;; P -> P
(define (copy-p p)
  (vector-map vector-copy p))

;; (node -> (U #t #f))
(define (node-empty? node)
  (zero? (node-n node)))

;; (node -> node)
(define (node-copy node)
  (make-node (node-n node) (vector-map vector-copy (node-p node))))

