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
  (export bp^)
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

  )
