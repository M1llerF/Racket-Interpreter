#lang racket

; Tests for: General Interpreter Functionality in startEval
; Description: This file contains comprehensive test cases to validate the core 
; functionality of the `startEval` interpreter. It includes tests for constants, 
; variable lookups, recursive bindings (`letrec`), and quote expressions.
;
; Author: Miller Fourie
; Last Modified: 2024-12-06
;
; Note: These tests demonstrate the correctness of fundamental features of the interpreter, 
; including handling literals, environment lookups, recursion via `letrec`, and quoted 
; expressions. Edge cases and expected results for various scenarios are also covered.

(provide interpreter-tests)

(require rackunit)
(require rackunit/text-ui)

(require "../src/interpreter.rkt")  ; Import the main interpreter code

(define interpreter-tests
  (test-suite
  "Interpreter Tests"
  ; Test constants and numbers
  (test-case "Constant Numbers"
    (check-equal? (startEval 42) 42))

  ; Test variable lookup
  (test-case "Variable Lookup"
    (define env '((x . 10) (y . 20)))
    (check-equal? (startEval 'x env) 10)
    (check-equal? (startEval 'y env) 20))

  ; letrec factorial test
  (test-case "Letrec Factorial Test"
    (check-equal? (startEval '(letrec ((fact (lambda (x) (if (= x 0) (quote 1) (* x (fact (- x 1)))))))
                              (fact 10))) 3628800))
  ; Test quote expressions
  (test-case "Quote Expressions"
    (check-equal? (startEval '(quote a)) 'a)
    (check-equal? (startEval '(quote (1 2 3))) '(1 2 3)))
))

(run-tests interpreter-tests)
