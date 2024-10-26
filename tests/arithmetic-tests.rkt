#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "../src/interpreter.rkt")  ; Import the main interpreter code

(define arithmetic-tests
  (test-suite
   "Arithmetic Tests"
   ;; Test arithmetic operations
   (test-case "Arithmetic Operations"
     (check-equal? (startEval '(+ 3 4) '()) 7)
     (check-equal? (startEval '(- 10 5) '()) 5)
     (check-equal? (startEval '(* 6 7) '()) 42)
     (check-equal? (startEval '(/ 20 4) '()) 5)
     (check-exn exn:fail? (lambda () (startEval '(/ 1 0) '()))))  ; Test division by zero

   ;; Test BEDMAS order of operations
   (test-case "BEDMAS Order of Operations"
     (check-equal? (startEval '(+ (* 2 3) 4) '()) 10)  ; 2 * 3 + 4 = 10
     (check-equal? (startEval '(* (+ 2 3) 4) '()) 20)  ; (2 + 3) * 4 = 20
     (check-equal? (startEval '(- (/ 20 5) 2) '()) 2)   ; 20 / 5 - 2 = 2
     (check-equal? (startEval '(/ (+ 10 10) (* 2 2)) '()) 5))  ; (10 + 10) / (2 * 2) = 5
))

(run-tests arithmetic-tests)
