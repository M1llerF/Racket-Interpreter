#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "../src/interpreter.rkt")  ; Import the main interpreter code

(define interpreter-tests
  (test-suite
   "Interpreter Tests"
   ;; Test constants and numbers
   (test-case "Constant Numbers"
     (check-equal? (startEval 42 '()) 42))

   ;; Test variable lookup
   (test-case "Variable Lookup"
     (define env '((x . 10) (y . 20)))
     (check-equal? (startEval 'x env) 10)
     (check-equal? (startEval 'y env) 20))

   ;; Test quote expressions
   (test-case "Quote Expressions"
     (check-equal? (startEval '(quote a) '()) 'a)
     (check-equal? (startEval '(quote (1 2 3)) '()) '(1 2 3)))
))

(run-tests interpreter-tests)
