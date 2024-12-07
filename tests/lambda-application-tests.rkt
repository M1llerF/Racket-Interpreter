#lang racket

; Tests for: Lambda Application in startEval
; Description: This file contains test cases to validate the handling of lambda expressions 
; and their applications in the `startEval` interpreter. It tests basic lambda application, 
; multiple arguments, nested lambdas, lambda applications within environments, higher-order 
; functions, function composition, and complex lambda body expressions.
;
; Author: Miller Fourie
; Last Modified: 2024-12-06
;
; Note: These tests ensure the correct evaluation and application of lambda expressions, 
; including support for edge cases such as nested evaluations, higher-order function applications, 
; and environment-based bindings.

(provide lambda-application-tests)

(require rackunit)
(require rackunit/text-ui)

(require "../src/interpreter.rkt")  ; Import the main interpreter code

(define lambda-application-tests
  (test-suite
  "Lambda Application Tests"

  ; Test simple lambda application
  (test-case "Simple Lambda Application"
    (check-equal? (startEval '((lambda (x) (+ x 3)) 7)) 10)
    (check-equal? (startEval '((lambda (y) (* y 2)) 5)) 10))

  ; Test lambda with multiple arguments
  (test-case "Lambda with Multiple Arguments Application"
    (check-equal? (startEval '((lambda (a b) (+ a b)) 3 4)) 7)
    (check-equal? (startEval '((lambda (x y) (- x y)) 10 6)) 4))

  ; Test applying lambda returned by another lambda
  (test-case "Lambda Returning Lambda Application"
    (check-equal? (startEval '(((lambda (x) (lambda (y) (+ x y))) 4) 6)) 10)
    (check-equal? (startEval '(((lambda (a) (lambda (b) (* a b))) 3) 5)) 15))

  ; Test lambda applied in an environment with bound variables
  (test-case "Lambda Application in Environment with Variables"
    (let ([env '((m . 7) (n . 3))])
      (check-equal? (startEval '((lambda (x) (+ x m)) 5) env) 12)
      (check-equal? (startEval '((lambda (y) (* y n)) 4) env) 12)))

  ; Test applying lambda that involves nested lambda evaluations
  (test-case "Nested Lambda Application"
    (check-equal? (startEval '((lambda (x) ((lambda (y) (* x y)) 2)) 3)) 6)
    (check-equal? (startEval '((lambda (a) ((lambda (b) (+ a b)) 4)) 5)) 9))

  ; Test lambda application with complex body expression
  (test-case "Lambda Application with Complex Body Expression"
    (check-equal? (startEval '((lambda (x) (if (> x 10) (* x 3) (+ x 5))) 8)) 13)
    (check-equal? (startEval '((lambda (y) (if (< y 2) (* y 4) (- y 1))) 5)) 4))

  ; Test higher-order function using lambda application
; Test higher-order function using lambda application
(test-case "Higher-Order Function Application"
  (check-equal? (startEval '(((lambda (f) (f 6)) (lambda (z) (+ z 2))))) 8)
  (check-equal? (startEval '(((lambda (g) (g 3)) (lambda (w) (* w 5))))) 15))


  ; Test application with lambda bound to a variable
  (test-case "Lambda Application with Variable Binding"
    (let ([env '((double . (lambda (x) (* x 2))))])
      (check-equal? (startEval '(double 10) env) 20)
      (check-equal? (startEval '(double 7) env) 14)))

  ; Test lambda application with function composition
  (test-case "Lambda Application with Function Composition"
    (check-equal? (startEval '(((lambda (f g) (lambda (x) (f (g x)))) (lambda (a) (+ a 1)) (lambda (b) (* b 2))) 3)) 7)
    (check-equal? (startEval '(((lambda (h k) (lambda (y) (h (k y)))) (lambda (c) (* c 3)) (lambda (d) (+ d 4))) 2)) 18))

; Test lambda applying twice
  (test-case "Lambda Apply Twice"
    (check-equal? (startEval '(let ((apply-twice (lambda (f x) (f (f x)))))
                                (apply-twice (lambda (y) (+ y 1)) 5))) 7))))

; Run tests and output results
(run-tests lambda-application-tests)
