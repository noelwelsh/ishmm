#lang scheme/base

(require
 scheme/unit
 (planet schematics/schemeunit:3/test)
 (planet schematics/numeric:1/vector)
 "sigs.ss"
 "bp.ss"
 "nodes/dirichlet-node.ss")

(define-unit node-config@
  (import)
  (export node-config^)
  (define p #(1 1)))

(define-unit bp-config@
  (import)
  (export bp-config^)
  (define c 1)
  (define gamma 4))

(define-values/invoke-unit/infer
  (export node^ bp^)
  (link node-config@ bp-config@ dirichlet-node@ bp@))

(define e 0.00001)

(define/provide-test-suite bp-tests
  (test-case
   "bp-sample-transitions for new BP"
   (define-values (ts uv-ts) (bp-sample-transitions (create-bp)))
   (check > uv-ts 0)
   (check = (vector-length ts) 0))

  (test-case
   "bp-sample-weights for new BP"
   (define-values (ws uv-ws) (bp-sample-weights (create-bp) 4))
   (check = (vector-length ws) 0)
   (check = (vector-length uv-ws) 4)
   (check-false (= (vector-ref uv-ws 0) (vector-ref uv-ws 1))))

  (test-case
   "bp-update creates expected BP"
   (define bp (bp-update (create-bp)
                         #(0 1 0 1 0 1)
                         #(0 1 0 2 0 3)))
   (define node-0 (bp-node-ref bp 0))
   (define node-1 (bp-node-ref bp 1))
   (define node-2 (bp-node-ref bp 2))
   (define node-3 (bp-node-ref bp 3))
   (define node-4 (bp-node-ref bp 4))
   (check-= (node-likelihood node-4 1) (node-prior-likelihood 1) 0)
   (check > (node-likelihood node-3 1) (node-prior-likelihood 1))  
   (fail "Not implemented"))

  (test-case
   "bp-update compacts node indices"
   (fail "Not implemented"))

  )
