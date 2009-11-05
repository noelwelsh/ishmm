#lang scheme/base

(require scheme/unit
         (planet schematics/schemeunit:3)
         (planet schematics/numeric:1/vector)
         "../sigs.ss"
         "dirichlets-node.ss")

(define p (vector-copy #(#(1 1) #(1 1))))

(define-values/invoke-unit dirichlets-node@
  (import node-config^)
  (export node^))

(define/provide-test-suite dirichlets-node-tests
  (test-case
   "create-node"
   (let ([c1 (node-add (create-node) #(0 0))]
         [c2 (create-node)])
     (check-equal? (node-n c1) 1)
     (check-equal? (node-params c1) #(#(2 1) #(2 1)))
     (check-equal? (node-n c2) 0)
     (check-equal? (node-params c2) #(#(1 1) #(1 1)))))

  (test-case
   "create-node copies p"
   (let ([c (create-node)])
     (vector-set! p 0 #(4 4))
     (check-equal? (node-params c) #(#(1 1) #(1 1)))
     (vector-set! p 0 #(1 1))))

  (test-case
   "node-add"
   (let ([c (node-add (create-node) #(0 1))])
     (check-equal? (node-params c) #(#(2 1) #(1 2)))
     (check-equal? (node-n c) 1)))

  (test-case
   "node-remove"
   (let ([c (node-remove (node-add (create-node) #(0 1)) #(0 1))])
     (check-equal? (node-params c) #(#(1 1) #(1 1)))
     (check-equal? (node-n c) 0)))
  
  (test-case
   "node-likelihood"
   (let ([c (node-add (create-node) #(0 1))])
     (check-equal? (node-likelihood c #(0 1)) (* 2/3 2/3))
     (check-equal? (node-likelihood c #(1 0)) (* 1/3 1/3))))

  (test-case
   "node-prior-likelihood"
   (check-equal? (node-prior-likelihood #(0 1)) 1/4)
   (check-equal? (node-prior-likelihood #(1 0)) 1/4))
  
  )
