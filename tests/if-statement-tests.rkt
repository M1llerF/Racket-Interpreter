#lang racket
(provide if-statement-tests)

(require rackunit)
(require rackunit/text-ui)

(require "../src/interpreter.rkt")  ; Import the main interpreter code

(define if-statement-tests
  (test-suite
  "If Statement Tests"

  ; Test simple true condition
  (test-case "Simple If - True Condition"
    (check-equal? (startEval '(if #t 1 2)) 1))

  ; Test simple false condition
  (test-case "Simple If - False Condition"
    (check-equal? (startEval '(if #f 1 2)) 2))

  ; Test with numerical comparison as condition
  (test-case "If with Numerical Comparison"
    (check-equal? (startEval '(if (> 5 3) 'yes 'no)) 'yes)
    (check-equal? (startEval '(if (< 3 1) 'yes 'no)) 'no))

  ; Test nested if statements
  (test-case "Nested If Statements"
    (check-equal? (startEval '(if #t (if #f 3 4) 2)) 4)
    (check-equal? (startEval '(if #f 1 (if #t 5 6))) 5))

  ; Test if with variable bindings
  (test-case "If with Variables"
    (let ([env '((a . 3) (b . 7))])
      (check-equal? (startEval '(if (> b a) 'greater 'less) env) 'greater)
      (check-equal? (startEval '(if (< a b) 'smaller 'not-smaller) env) 'smaller)))

  ; Test if without else branch (should return an unspecified value)
  (test-case "If without Else Branch"
    (check-equal? (startEval '(if #t 42)) 42)
    (check-equal? (startEval '(if #f 42)) '())))) ; #f branch should return '()

; Run tests and output results
(run-tests if-statement-tests)
