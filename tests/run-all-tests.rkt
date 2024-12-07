#lang racket

(require rackunit)
(require rackunit/text-ui)

; Require all test files
(require "arithmetic-tests.rkt")
(require "if-statement-tests.rkt")
(require "interpreter-tests.rkt")
(require "lambda-application-tests.rkt")
(require "lambda-tests.rkt")
(require "let-letrec-tests.rkt")
(require "list-operations-tests.rkt")
(require "relational-tests.rkt")

; Create a test suite that combines all tests
(define all-tests
  (test-suite
  "All Tests"
  arithmetic-tests  ; Use the test suite directly (no parentheses)
  if-statement-tests
  interpreter-tests
  lambda-application-tests
  lambda-tests
  let-letrec-tests
  list-operations-tests
  relational-tests))

; Run the combined test suite
(displayln "Running all tests...")
(run-tests all-tests)  ; Run the combined test suite
(displayln "All tests completed!")
