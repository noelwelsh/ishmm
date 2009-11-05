#lang scheme/base

(require
 (planet schematics/schemeunit:3/test)
 "dirichlet-node-test.ss"
 "dirichlets-node-test.ss")

(define/provide-test-suite all-tests
  dirichlet-node-tests
  dirichlets-node-tests
  )