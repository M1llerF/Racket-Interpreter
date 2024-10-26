#lang racket
(provide startEval) ; Provide startEval for testing files
; Main function
(define (startEval expr env)
  ; Dispatch based on the type of expression
  (cond
    [(number? expr) expr]  ; Return numbers directly
    [(symbol? expr) (lookup-symbol expr env)]  ; Look up variables
    [(pair? expr) (eval-list expr env)]  ; Handle lists (expressions)
    [else (error "Unsupported expression" expr)]))

; Environment handling
(define (lookup-symbol sym env)
  (let ([val (assoc sym env)])
    (if val
        (cdr val)
        (error "Variable not found" sym))))

; Evaluator: Evaluate lists
(define (eval-list expr env)
  (let ((op (car expr)))
    (cond
      [(member op '(+ - * /)) (eval-arith expr env)]  ; Arithmetic operations
      [(eq? op 'quote) (eval-quote expr)]  ; Quote expressions
      [else (apply (startEval op env) (map (lambda (e) (startEval e env)) (cdr expr)))])))


; Evaluating arithmetic operations
(define (eval-arith expr env)
  (let* ([arg1 (startEval (cadr expr) env)]
        [arg2 (startEval (caddr expr) env)])
    (case (car expr)
      [(+) (+ arg1 arg2)] ;
      [(-) (- arg1 arg2)]
      [(*) (* arg1 arg2)]
      [(/) (if (zero? arg2) ; Check for division by zero
              (error "Division by zero" expr)
              (/ arg1 arg2))]
      [else (error "Unsupported arithmetic operation" expr)]))) ; Else if 

; Evaluator: Handle quote expressions
(define (eval-quote expr)
  (if (and (pair? expr) (eq? (car expr) 'quote))
      (cadr expr)
      (error "Unsupported quote expression" expr)))