#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "../src/interpreter.rkt")  ; Import the main interpreter code

(define lambda-expression-tests
  (test-suite
  "Lambda Expression Tests"

  ; Test simple lambda expression
  (test-case "Simple Lambda Expression"
    (check-equal? (startEval '((lambda (x) (+ x 1)) 5) '()) 6)
    (check-equal? (startEval '((lambda (y) (* y 2)) 3) '()) 6))

  ; Test lambda with multiple arguments
  (test-case "Lambda with Multiple Arguments"
    (check-equal? (startEval '((lambda (x y) (+ x y)) 4 5) '()) 9)
    (check-equal? (startEval '((lambda (a b) (- a b)) 10 3) '()) 7))

  ; Test lambda returning another lambda
  (test-case "Lambda Returning Another Lambda"
    (check-equal? (startEval '(((lambda (x) (lambda (y) (+ x y))) 5) 10) '()) 15)
    (check-equal? (startEval '(((lambda (a) (lambda (b) (* a b))) 3) 4) '()) 12))

  ; Test lambda in an environment
  (test-case "Lambda in Environment"
    (let ([env '((a . 2) (b . 3))])
      (check-equal? (startEval '((lambda (x) (+ x a)) 4) env) 6)
      (check-equal? (startEval '((lambda (y) (* y b)) 5) env) 15)))

  ; Test nested lambdas
  (test-case "Nested Lambdas"
    (check-equal? (startEval '((lambda (x) ((lambda (y) (+ x y)) 3)) 2) '()) 5)
    (check-equal? (startEval '((lambda (a) ((lambda (b) (* a b)) 4)) 3) '()) 12))

  ; Test lambda with complex body expression
  (test-case "Lambda with Complex Body Expression"
    (check-equal? (startEval '((lambda (x) (if (> x 5) (* x 2) (+ x 3))) 4) '()) 7)
    (check-equal? (startEval '((lambda (y) (if (< y 3) (* y 5) (- y 2))) 2) '()) 10))

  ; Test lambda bound to a variable
  (test-case "Lambda Bound to Variable"
    (let ([env '((f . (lambda (x) (+ x 10))))])
    (check-equal? (startEval '(f 5) env) 15)))

  ; Test lambda used in higher-order function context
  (test-case "Lambda in Higher-Order Function Context"
    (check-equal? (startEval '(((lambda (f) (f 3)) (lambda (x) (+ x 4)))) '()) 7)
    (check-equal? (startEval '(((lambda (g) (g 2)) (lambda (y) (* y 5)))) '()) 10))))

; Run tests and output results
(run-tests lambda-expression-tests)
