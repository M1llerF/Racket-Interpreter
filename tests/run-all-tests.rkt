#lang racket

; Combined Tests for: startEval Interpreter
; Description: This file combines and executes all individual test suites for the 
; `startEval` interpreter. It ensures that all aspects of the interpreter, including 
; arithmetic operations, conditional statements, list operations, lambda expressions, 
; and local/recursive bindings, are thoroughly tested.
;
; Author: Miller Fourie
; Last Modified: 2024-12-06
;
; Note: This combined test suite is designed to streamline testing and provide a 
; comprehensive overview of the interpreter's functionality. All individual test 
; files are required and run within a unified framework to validate the entire 
; implementation.

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
