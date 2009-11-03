#lang scheme/base

(require (planet schematics/schemeunit:3)
         "util.ss")

(define (make-generator)
  (vector->pseudo-random-generator #(1 1 1 1 1 1)))

(define/provide-test-suite util-tests

  (test-case
   "multinomial-sample all zeros"
   (check-equal? (multinomial-sample #(0 0 0 0 0)) #f))

  (test-case
   "multinomial-sample one one"
   (check-equal? (multinomial-sample #(0 1 0 0 0)) 1))

  (test-case
   "multinomial-sample corner cases"
   (check-equal? (multinomial-sample #(1 0 0 0 0)) 0)
   (check-equal? (multinomial-sample #(0 0 1 0 0)) 2)
   (check-equal? (multinomial-sample #(0 0 0 0 1)) 4))
  
  (test-case
   "multinomial-sample"
   (let ([p #(1/2 1/4 1/4)]
         [g (make-generator)])
     (check-equal? (for/list ([i (in-range 10)])
                             (multinomial-sample p g))
                   '(0 1 0 0 0 2 0 2 0 1))))

  (test-case
   "multinomial-sample w/ empty clusters"
   (let ([p #(1/2 0 0 1/4 0 1/4)]
         [g (make-generator)])
     (check-equal? (for/list ([i (in-range 10)])
                             (multinomial-sample p g))
                   '(0 3 0 0 0 5 0 5 0 3))))

  (test-case
   "multinomial-sample w/ sum < 1"
   (let ([p #(1/2 1/4)]
         [g (make-generator)])
     (check-equal? (for/list ([i (in-range 10)])
                             (multinomial-sample p g))
                   '(0 1 0 0 0 #f 0 #f 0 1))))

  (test-case
   "multinomial-sample raises exn if sum > 1"
   (check-exn exn:fail?
              (lambda ()
                (multinomial-sample #(1 0 3/4)))))
  )

