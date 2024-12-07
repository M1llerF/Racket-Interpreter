#lang racket

(provide list-operations-tests)

(require rackunit)
(require rackunit/text-ui)

(require "../src/interpreter.rkt")  ; Import the main interpreter code

(define list-operations-tests
  (test-suite
  "List Operations Tests"

  ; Test car operation
  (test-case "car Operation"
    (check-equal? (startEval '(car '(1 2 3))) 1)
    (check-equal? (startEval '(car '((a b) c d))) '(a b))
    (check-exn exn:fail? (lambda () (startEval '(car)))))

  ; Test cdr operation
  (test-case "cdr Operation"
    (check-equal? (startEval '(cdr '(1 2 3))) '(2 3))
    (check-equal? (startEval '(cdr '((a b) c d))) '(c d))
    (check-exn exn:fail? (lambda () (startEval '(cdr)))))

  ; Test cons operation
  (test-case "cons Operation"
    (check-equal? (startEval '(cons 1 '(2 3))) '(1 2 3))
    (check-equal? (startEval '(cons '(a b) '(c d))) '((a b) c d))
    (check-equal? (startEval '(cons 1 2)) '(1 . 2)))

  ; Test pair? operation
  (test-case "pair? Operation"
    (check-equal? (startEval '(pair? '(1 2 3))) #t)
    (check-equal? (startEval '(pair? '())) #f)
    (check-equal? (startEval '(pair? '(a))) #t)
    (check-equal? (startEval '(pair? 1)) #f))))


; Run tests and output results
(run-tests list-operations-tests)