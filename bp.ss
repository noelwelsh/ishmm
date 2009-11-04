#lang scheme/unit

(require
 (planet williams/science/random-distributions/beta)
 (planet schematics/numeric:1/for)
 "base.ss"
 "sigs.ss")

(import
 node^
 bp-config^)

(export
 bp^)

;; struct bp : Number Number (Vectorof Number) (Vectorof Node)
;;
;; n is the number of posterior updates, and hence the
;; mixing proportion between the discrete and continuous
;; parts of the BP
;;
;; weights is the vector of weights given to the nodes
;;
;; nodes is the already sampled nodes, discrete part of the BP.
(define-struct bp (c n weights nodes))


;; -> BP
(define (create-bp)
  (make-bp 0 (vector) (vector)))

;; BP -> (values (Vectorof {0,1}) Natural)
;;
;; Returns a binary vector of edges to sampled states, and
;; the number of unvisited nodes that this transition
;; vector is connected to.
(define (bp-sample-transitions p)
  (match-define (struct bp [c n weights nodes]) p)
  (values
   (vector-map
    (lambda (w)
      (define p (/ w (+ c n)))
      (if (< (random) p)
          1
          0))
    weights)
   (random-poisson (/ (* c gamma) (+ c n)))))

;; BP -> (values (Vectorof [0,1]) (Vectorof [0,1]))
;;
;; Returns two vectors of weights. The indices of the first
;; vector indicate the node (as returned by
;; bp-node-ref). The second vector is weights given to
;; unsampled nodes. The indices are arbitrary.
(define (bp-sample-weights p)
  (match-define (struct bp [c n weights nodes]) p)
  (values
   (vector-map
    (lambda (w)
      (for/sum ([i (in-range w)])
        (random-beta 1 (+ c n -1))))
    weights)
   (for/vector ([i (random-poisson (/ (* c gamma) (+ c n)))])
               ;; TODO! Check this!
     (random-beta 1 (+ c n)))))

;; BP (Vectorof Obs) (Vectorof Natural) -> BP
;;
;; Assignments is a vector of indices, indicating to which node each observation is assigned. These indices are *not* retained in the returned BP -- data is assigned to nodes so that there are no gaps in the indices.
(define (bp-update bp data assignments)
  ;; (Hashof Natural Natural)
  (define idx-lookup (make-hash))
  ;; (Hashof Natural Node)
  (define nodes (make-hash))
  ;; Natural
  ;; The number of unique indices in assignments. Calculating it has the side-effect of filling idx-lookup and nodes
  (define n-indices
    (for/fold ([next-idx 0])
        ([o (in-vector data)]
         [i (in-vector assignments)])
      (if (hash-has-key? idx-lookup i)
          (let ([idx (hash-ref idx-lookup i)])
            (hash-update! nodes idx (lambda (node) (node-add node o)))
            next-idx)
          (begin (hash-set! idx-lookup i next-idx)
                 (hash-set! nodes next-idx (node-add (create-node) o))
                 (add1 next-idx)))))
  (for/vector ([i n-indices])
    (hash-ref nodes i)))

;; BP Natural -> Node
(define (bp-node-ref p idx)
  (match-define (struct bp [c n weights nodes]) p)
  (if (< idx (vector-length nodes))
      (vector-ref nodes idx)
      (create-node)))