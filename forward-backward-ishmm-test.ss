#lang scheme/base

(require
 scheme/unit
 (planet schematics/schemeunit:3/test)
 (planet schematics/numeric:1/vector)
 "base.ss"
 "sigs.ss"
 "bp.ss"
 "ishmm.ss"
 "forward-backward.ss"
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
  (export hmm^ forward-backward^)
  (link node-config@ bp-config@ dirichlet-node@ bp@ ishmm@ forward-backward@))


(define/provide-test-suite forward-backward-ishmm-tests
  ;; Here we just check we don't crash
  (test-case
   "new ishmm w/ forward-backward"
   (let loop ()
     (with-handlers ([exn:ishmm:no-transitions? (lambda (e) (loop))])
       (sample (smooth (create-hmm) #(0 1 0 1))))))

  (test-case
   "ishmm with data"
   (display (sample (smooth (hmm-update (create-hmm) #(0 1 0 1 0 1) #(0 1 0 1 0 1)) #(0 1 0 1 0 1))))
   (fail "Not implemented"))
  )