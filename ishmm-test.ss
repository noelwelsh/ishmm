#lang scheme/base

(require
 scheme/unit
 (planet schematics/schemeunit:3/test)
 (planet schematics/numeric:1/vector)
 "sigs.ss"
 "bp.ss"
 "ishmm.ss"
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
  (export hmm^)
  (link node-config@ bp-config@ dirichlet-node@ bp@ ishmm@))

(define e 0.00001)

(define/provide-test-suite ishmm-tests
  (test-case
   "hmm-initial-probabilities exist and sum to one"
   (define ps (hmm-initial-probabilities (create-hmm)))
   (check > (vector-length ps) 0)
   (check-= (vector-sum ps) 1.0 e))

  (test-case
   "hmm-initial-probabilities constant"
   (define hmm (create-hmm))
   (define ps1 (hmm-initial-probabilities hmm))
   (define ps2 (hmm-initial-probabilities hmm))
   (check-equal? ps2 ps1))

  (test-case
   "hmm-transition-probabilities exist and sum to one"
   (define ps (hmm-transition-probabilities (create-hmm) 0))
   (check > (vector-length ps) 0)
   (check-= (vector-sum ps) 1.0 e))

  (test-case
   "hmm-transition-probabilities constant"
   (define hmm (create-hmm))
   (define ps1 (hmm-transition-probabilities hmm 1))
   (define ps2 (hmm-transition-probabilities hmm 1))
   (check-equal? ps1 ps2))

  (test-case
   "hmm-update correctly updates hmm"
   (define hmm (hmm-update (create-hmm) #(0 1 0 0 1) #(1 0 1 1 0)))
   (define os (hmm-observation-probabilities hmm 0))
   (define ps (hmm-transition-probabilities hmm 0))

   ;; Indices are shuffled by bp-update
   (check-equal? os (vector 4/5 1/4))
   (display ps)(newline)
   (check > (vector-length ps) 0)
   (check-= (vector-sum ps) 1.0 e))
  )
