#lang racket

; Tests for: Let and Letrec Bindings in startEval
; Description: This file contains test cases for validating the implementation of `let` 
; and `letrec` expressions in the `startEval` interpreter. The tests include simple 
; bindings, nested expressions, shadowing, multiple bindings, recursive definitions, 
; mutual recursion, interdependent bindings, delayed evaluation, and complex nested 
; scenarios combining `let` and `letrec`.
;
; Author: Miller Fourie
; Last Modified: 2024-12-06
;
; Note: These tests ensure that local and recursive bindings are correctly evaluated 
; and handle various edge cases, including mutual recursion, interdependencies, and 
; delayed evaluation. They also validate the proper scoping and shadowing behavior of 
; `let` and `letrec`.

(provide let-letrec-tests)

(require rackunit)
(require rackunit/text-ui)

(require "../src/interpreter.rkt")  ; Import the main interpreter code

(define let-letrec-tests
  (test-suite
  "Local Binding Tests"

  ; Simple let expression
  (test-case "Simple Let Binding"
    (check-equal? (startEval '(let ((x 3)) (+ x 5))) 8)
    (check-equal? (startEval '(let ((y 10)) (* y 2))) 20))

  ; Nested let expressions
  (test-case "Nested Let Binding"
    (check-equal? (startEval '(let ((a 2)) (let ((b 3)) (+ a b)))) 5)
    (check-equal? (startEval '(let ((x 4)) (let ((y (+ x 2))) (* x y)))) 24))

  ; let expression shadowing
  (test-case "Let Binding Shadowing"
    (check-equal? (startEval '(let ((x 5)) (let ((x 10)) (+ x 3)))) 13)
    (check-equal? (startEval '(let ((x 7)) (let ((x (* x 2))) (- x 3)))) 11))

  ; let with multiple bindings
  (test-case "Let with Multiple Bindings"
    (check-equal? (startEval '(let ((a 3) (b 4) (c 5)) (+ a b c))) 12)
    (check-equal? (startEval '(let ((x 1) (y 2) (z 3)) (* x y z))) 6))

  ; letrec with recursive definition
  (test-case "Simple Letrec"
    (check-equal? (startEval '(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))) 120)
    (check-equal? (startEval '(letrec ((fibo (lambda (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))) (fibo 6))) 8))

  ; letrec with mutual recursion
  (test-case "Letrec with Mutual Recursion"
    (check-equal? (startEval '(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                                      (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
                              (even? 4))) #t)
    (check-equal? (startEval '(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                                      (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
                              (odd? 5))) #t))

  ; letrec that binds values depending on each other
  (test-case "Letrec with Interdependent Bindings"
    (check-equal? (startEval '(letrec ((a (+ 1 b))
                                       (b 5))
                              (+ a b))) 11)
    (check-equal? (startEval '(letrec ((x (* 2 y))
                                       (y 3))
                              (- x y))) 3))

  ; Complex nested letrec and let
  (test-case "Nested Letrec and Let"
    (check-equal? (startEval '(let ((x 2))
                                (letrec ((y (+ x 3))
                                         (z (lambda (n) (+ y n))))
                                  (z 4)))) 9)
    (check-equal? (startEval '(let ((a 10))
                                (letrec ((b (- a 3))
                                         (c (lambda (m) (* b m))))
                                  (let ((d 2))
                                    (c d))))) 14))

  ; letrec with delayed evaluation
  (test-case "Letrec with Delayed Evaluation"
    (check-equal? (startEval '(letrec ((a (delay (+ 2 3)))
                                       (b (force a)))
                              b)) 5)
    (check-equal? (startEval '(letrec ((x (delay (* 4 5)))
                                       (y (force x)))
                              y)) 20))))

; Run tests and output results
(run-tests let-letrec-tests)
