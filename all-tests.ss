#lang scheme/base

(require
 (planet schematics/schemeunit:3/test)
 "forward-backward-test.ss"
 "forward-backward-ishmm-test.ss"
 "bp-test.ss"
 "ishmm-test.ss"
 "util-test.ss"
 (prefix-in nodes: "nodes/all-tests.ss"))

(define/provide-test-suite all-tests
  forward-backward-tests
  forward-backward-ishmm-tests
  bp-tests
  ishmm-tests
  util-tests
  nodes:all-tests)