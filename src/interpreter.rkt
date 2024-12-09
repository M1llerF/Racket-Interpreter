#lang racket

; Project: startEval - A Racket Interpreter
; Description: This project implements an interpreter for a subset of Racket within Racket. 
; The main functionality is the evaluation of Racket programs using the `startEval` function. 
; This includes handling constants, variables, arithmetic, relational operators, lists, 
; conditional expressions, lambdas, function applications, and local bindings.
;
; Author: Miller Fourie
; Contact: millerfwork@gmail.com
; Last Modified: 2024-12-06
;
; Course: CPSC 3740 - Fall 2024
; Instructor: Dr. Howard Cheng
; Specifications: This interpreter was designed as per the guidelines provided in the 
; course project. The implementation avoids using Racket's `eval` and focuses on creating 
; a custom evaluation function with no side effects.

; To run code with startEval:
; 1. Pass an expression to evaluate and an environment with variable bindings.
; 2. Example:
;    (startEval '(+ x y) '((x . 10) (y . 20)))
;    This evaluates the expression (+ x y) with x = 10 and y = 20.
; 3. Output for the example above: 30

(provide startEval) ; Provide startEval for testing files
; Main function
(define (startEval expr (env '()))
  ; Dispatch based on the type of expression
  (cond
    [(number? expr) expr] ; Return numbers directly
    [(boolean? expr) expr] ; Return booleans directly
    [(null? expr) '()] ; Return empty list directly
    [(symbol? expr) (lookup-symbol expr env)] ; Look up variables in the environment
    [(pair? expr) (eval-list expr env)] ; Handle compound expressions (lists)
    [else (error "Unsupported expression" expr)])) ; Handle unsupported expressions

; Environment handling: Look up the value of the variable in the environment
(define (lookup-symbol sym env)
  (let ([val (assoc sym env)])
    (if val
        (let ([v (cdr val)])
          ; Handle values that need to be forced or evaluated
          (cond
            [(promise? v) (force v)] ; Force promises (used in letrec)
            [(and (pair? v) (eq? (car v) 'lambda))
            ; Evaluate the lambda expression to get a closure
            (eval-lambda v env)]
            [else v]))
        (error "Variable not found" sym))))

; Evaluator: Evaluate lists
(define (eval-list expr env)
  (let ([op (car expr)])
    (cond
      ; Handle arithmetic operations
      [(member op '(+ - * /)) (eval-arith expr env)]
      ; Handle relational operations
      [(member op '(= < > <= >= equal?)) (eval-relational expr env)]
      ; Handle list operations
      [(member op '(car cdr cons pair?)) (eval-list-ops expr env)]
      ; Handle 'quote expressions (returning data literally)
      [(eq? op 'quote) (eval-quote expr)]
      ; Handle conditional if expressions
      [(eq? op 'if) (eval-if expr env)]
      ; Handle lambda expressions
      [(eq? op 'lambda) (eval-lambda expr env)]
      ; Handle let expressions for variable binding
      [(eq? op 'let) (eval-let expr env)]
      ; Handle letrec expressions for recursive variable binding
      [(eq? op 'letrec) (eval-letrec expr env)]
      ; Handle delayed evaluation
      [(eq? op 'delay) (eval-delay expr env)]
      ; Handle forcing a delayed evaluation
      [(eq? op 'force) (eval-force expr env)]
      ; Handle function application when the operator is a pair
      [(pair? op)
       (let ([func (startEval op env)]
             [args (map (lambda (e) (startEval e env)) (cdr expr))])
         (cond
           [(and (or (procedure? func) (and (list? func) (eq? (car func) 'closure)))
                 (not (null? args)))
            (apply-function func args)]
           [(null? args) func]
           [else
            (error
             "Application: not a procedure; expected a procedure that can be applied to arguments"
             func)]))]
      [else
       ; Evaluate the operator and operands
       (let ([func (startEval op env)]
             [args (map (lambda (e) (startEval e env)) (cdr expr))])
         (apply-function func args))])))

; Evaluator: Handle quote expressions (returns the quoted data)
(define (eval-quote expr)
  (if (and (pair? expr) (eq? (car expr) 'quote))
      (cadr expr)
      (error "Unsupported quote expression" expr)))

; Evaluator: Handle if expressions (conditioanl evaluation)
(define (eval-if expr env)
  (let* ([condition (startEval (cadr expr) env)]
         [then-branch (caddr expr)]
         [else-branch (if (> (length expr) 3)
                          (cadddr expr)
                          '())])
    (if condition
        (startEval then-branch env)
        (if (> (length expr) 3)
            (startEval else-branch env)
            '())))) ; Return '() if no else-branch is provided

; Evaluator: Handle arithmetic operations
(define (eval-arith expr env)
  (let* ([args (map (lambda (e) (startEval e env)) (cdr expr))])
    (case (car expr)
      [(+)
       (apply + args)]
      [(-)
       (cond
         [(null? args) (error "Subtraction requires at least one operand")]
         [else (apply - args)])]
      [(*) (apply * args)]
      [(/)
       (cond
         [(null? args) (error "Division requires at least one operand")]
         [(member 0 (cdr args)) (error "Division by zero")]
         [else (apply / args)])]
      [else (error "Unsupported arithmetic operation" expr)])))

; Evaluator: Handle relational operations
(define (eval-relational expr env)
  (let* ([arg1 (startEval (cadr expr) env)]
         [arg2 (startEval (caddr expr) env)])
    (case (car expr)
      [(=) (custom-equal? arg1 arg2)]
      [(<) (custom-less? arg1 arg2)]
      [(>) (custom-greater? arg1 arg2)]
      [(<=) (custom-less-or-equal? arg1 arg2)]
      [(>=) (custom-greater-or-equal? arg1 arg2)]
      [(equal?) (custom-equal? arg1 arg2)])))

; Custom implementation of equal?
(define (custom-equal? x y)
  (cond
    ; If both are numbers, check if they are equal
    [(and (number? x) (number? y)) (= x y)]

    ; If both are symbols, check if they are equal
    [(and (symbol? x) (symbol? y)) (eq? x y)]

    ; If both are empty lists, they are equal
    [(and (null? x) (null? y)) #t]

    ; If both are lists, compare each element recursively
    [(and (pair? x) (pair? y))
     (and (custom-equal? (car x) (car y)) ; Compare the heads of the lists
          (custom-equal? (cdr x) (cdr y)))] ; Recursively compare the tails

    ; If types don't match, they are not equal
    [else #f]))

; Custom implementation of less-than-or-equal
(define (custom-less-or-equal? x y)
  (cond
    ; Ensure both arguments are numbers
    [(and (number? x) (number? y))
     (let ([difference (- y x)]) (if (negative? difference) #f #t))]
    [else (error "Arguments must be numbers for comparison" x y)]))

; Custom implementation of greater-than-or-equal
(define (custom-greater-or-equal? x y)
  (cond
    ; Ensure both arguments are numbers
    [(and (number? x) (number? y))
     (let ([difference (- x y)]) (if (negative? difference) #f #t))]
    [else (error "Arguments must be numbers for comparison" x y)]))

; Custom implementation of less than using greater-than-or-equal
(define (custom-less? x y)
  (cond
    [(custom-greater-or-equal? x y) #f]
    [else #t]))

; Custom implementation of greater than using less-than-or-equal
(define (custom-greater? x y)
  (cond
    [(custom-less-or-equal? x y) #f]
    [else #t]))

; Evaluator: Handle list operations like car, cdr, cons, etc.
(define (eval-list-ops expr env)
  (let* ([arg1 (startEval (cadr expr) env)]
         [arg2 (if (> (length expr) 2)
                   (startEval (caddr expr) env)
                   '())])
    (case (car expr)
      [(car)
       (if (pair? arg1)
           (car arg1)
           (error "car: argument is not a pair" arg1))]
      [(cdr)
       (if (pair? arg1)
           (cdr arg1)
           (error "cdr: argument is not a pair" arg1))]
      [(cons) (cons arg1 arg2)]
      [(pair?) (if (pair? arg1) #t #f)]
      [else (error "Unsupported list operation" expr)])))

; Evaluator: Handle lambda expressions (function creation)
(define (eval-lambda expr env)
  (let ([params (cadr expr)]
        [body (caddr expr)])
    (list 'closure params body env)))

; Evaluator: Handle let expressions (variable binding)
(define (eval-let expr env)
  (let* ([bindings (cadr expr)]
         [body (caddr expr)]
         [new-bindings (map (lambda (binding)
                              (let ([var (car binding)]
                                    [val (startEval (cadr binding) env)])
                                (cons var val)))
                            bindings)]
         [new-env (append new-bindings env)])
    (startEval body new-env)))

; Evaluator: Handle letrec expressions (recursive variable binding)
(define (eval-letrec expr env)
  (letrec ([extended-env
            (append (map (lambda (binding)
                           (let ([var (car binding)]
                                 [expr (cadr binding)])
                             (cons var (delay (startEval expr extended-env)))))
                         (cadr expr))
                    env)])
    (startEval (caddr expr) extended-env)))


; Function application: Apply a function
(define (apply-function func args)
  (cond
    ; Built-in procedures
    [(procedure? func) (apply func args)]
    ; User-defined closures
    [(and (list? func) (eq? (car func) 'closure))
     (let* ([params (cadr func)]
            [body (caddr func)]
            [closure-env (cadddr func)]
            [new-env (append (zip params args) closure-env)])
       (if (= (length params) (length args))
           (startEval body new-env)
           (error "Incorrect number of arguments" func args)))]
    [else
     (error "Application: not a procedure; expected a procedure that can be applied to arguments"
            func)]))

; Helper function to pair parameters with arguments
(define (zip params args)
  (if (null? params)
      '()
      (cons (cons (car params) (car args)) (zip (cdr params) (cdr args)))))


; Evaluator: Handle delay expressions
(define (eval-delay expr env)
  (let ([delayed-expr (cadr expr)])
    (delay (startEval delayed-expr env))))

; Evaluator: Handle force expressions
(define (eval-force expr env)
  (let ([promise (startEval (cadr expr) env)])
    (if (promise? promise)
        (force promise)
        (error "force: expected a promise, got" promise))))
