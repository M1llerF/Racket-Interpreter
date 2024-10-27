#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "../src/interpreter.rkt")  ; Import the main interpreter code

(define relational-tests
  (test-suite
  "Relational Tests"

  ; Test simple relational operations
  (test-case "Simple Relational Operations"
    (check-equal? (startEval '(= 3 3) '()) #t)
    (check-equal? (startEval '(< 3 4) '()) #t)
    (check-equal? (startEval '(> 5 2) '()) #t)
    (check-equal? (startEval '(<= 4 4) '()) #t)
    (check-equal? (startEval '(>= 6 6) '()) #t)
    (check-equal? (startEval '(equal? '(1 2) '(1 2)) '()) #t)
    (check-equal? (startEval '(> 2 5) '()) #f))

  ; Test comparing nested lists
  (test-case "Comparing Nested Lists"
    (check-equal? (startEval '(equal? '(1 (2 3) 4) '(1 (2 3) 4)) '()) #t)
    (check-equal? (startEval '(equal? '(1 (2 3) 4) '(1 (2 4) 4)) '()) #f)
    (check-equal? (startEval '(equal? '((1) (2 3)) '((1) (2 3))) '()) #t)
    (check-equal? (startEval '(equal? '((1) (2 3)) '((1) (3 2))) '()) #f))

  ; Test relational operations with negative numbers
  (test-case "Relational Operations with Negative Numbers"
    (check-equal? (startEval '(< -3 4) '()) #t)
    (check-equal? (startEval '(> -5 -2) '()) #f)
    (check-equal? (startEval '(<= -4 -4) '()) #t)
    (check-equal? (startEval '(>= -6 -7) '()) #t))

  ; Test relational operations with mixed positive and negative numbers
  (test-case "Mixed Positive and Negative Numbers"
    (check-equal? (startEval '(< -3 3) '()) #t)
    (check-equal? (startEval '(> 3 -3) '()) #t)
    (check-equal? (startEval '(<= -5 5) '()) #t)
    (check-equal? (startEval '(>= 5 -5) '()) #t))

  ; Test relational operations involving zero
  (test-case "Relational Operations with Zero"
    (check-equal? (startEval '(= 0 0) '()) #t)
    (check-equal? (startEval '(< 0 1) '()) #t)
    (check-equal? (startEval '(> 0 -1) '()) #t)
    (check-equal? (startEval '(<= 0 0) '()) #t)
    (check-equal? (startEval '(>= 0 0) '()) #t))

  ; Test relational operations with deeper nested structures
  (test-case "Deeper Nested Structures"
    (check-equal? (startEval '(equal? '((1 2) (3 (4 5))) '((1 2) (3 (4 5)))) '()) #t)
    (check-equal? (startEval '(equal? '((1 2) (3 (4 5))) '((1 2) (3 (4 6)))) '()) #f)
    (check-equal? (startEval '(equal? '(1 (2 (3 (4)))) '(1 (2 (3 (4))))) '()) #t)
    (check-equal? (startEval '(equal? '(1 (2 (3 (4)))) '(1 (2 (3 (5))))) '()) #f))

  ; Test relational operations involving variables
  (test-case "Relational Operations with Variables"
    (let ([env '((a . 5) (b . -3) (c . 0))])
      (check-equal? (startEval '(= a 5) env) #t)
      (check-equal? (startEval '(< b a) env) #t)
      (check-equal? (startEval '(> a c) env) #t)
      (check-equal? (startEval '(<= b b) env) #t)
      (check-equal? (startEval '(>= c b) env) #t)))))

; Run tests and output results
(run-tests relational-tests)