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
      [(member op '(= < > <= >= equal?)) (eval-relational expr env)]  ; Handle relational operations

      [(eq? op 'quote) (eval-quote expr)]  ; Quote expressions
      [else (apply (startEval op env) (map (lambda (e) (startEval e env)) (cdr expr)))])))

; Evaluator: Handle quote expressions
(define (eval-quote expr)
  (if (and (pair? expr) (eq? (car expr) 'quote))
      (cadr expr)
      (error "Unsupported quote expression" expr)))

; Evaluating arithmetic operations
(define (eval-arith expr env)
  (let* ([arg1 (startEval (cadr expr) env)]
        [arg2 (startEval (caddr expr) env)])
    (case (car expr)
      [(+) (custom-add arg1 arg2)] ;
      [(-) (custom-subtract arg1 arg2)]
      [(*) (custom-multiply arg1 arg2)]
      [(/) (custom-divide arg1 arg2)] ; Check for division by zero
      [else (error "Unsupported arithmetic operation" expr)]))) ; Else if 

; Custom implementation of addition
(define (custom-add x y)
  (cond
    [(zero? y) x] ; Base case: if y is 0, return x (adding 0 doesn't change the value)
    [(positive? y) (custom-add (add1 x) (sub1 y))] ; If y is positive, increment x and decrement y
    [(negative? y) (custom-add (sub1 x) (add1 y))] ; If y is negative, decrement x and increment y
    [else (error "Invalid arguments for addition" x y)]))

; Custom implementation of subtraction
(define (custom-subtract x y)
  (cond
    [(zero? y) x] ; Base case: if y is 0, return x (subtracting 0 doesn't change the value)
    [(positive? y) (custom-subtract (sub1 x) (sub1 y))] ; If y is positive, decrement x and decrement y
    [(negative? y) (custom-subtract (add1 x) (add1 y))] ; If y is negative, increment x and increment y
    [else (error "Invalid arguments for subtraction" x y)]))

; Custom implementation of multiplication
(define (custom-multiply x y)
  (define (multiply-helper a b result)
    (cond
      [(zero? b) result] ; Base case: if b is 0, return the result
      [(positive? b) (multiply-helper a (sub1 b) (custom-add result a))] ; 
      [(negative? b) (multiply-helper a (add1 b) (custom-subtract result a))]
      [else (error "Invalid arguments for multiplication" a b)]))
  (multiply-helper x y 0))

; Custom implementation of division
(define (custom-divide x y)
  (if (zero? y)
      (error "Division by zero" x y)
      (let loop ([numerator x] [denominator y] [quotient 0])
        (cond
          [(custom-less? numerator denominator) quotient]
          [else (loop (custom-subtract numerator denominator) denominator (add1 quotient))]))))

; Custom implementation of relational operations
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
    (and (custom-equal? (car x) (car y))  ; Compare the heads of the lists
          (custom-equal? (cdr x) (cdr y)))] ; Recursively compare the tails

    ; If types don't match, they are not equal
    [else #f]))



; Custom implementation of less-than-or-equal
(define (custom-less-or-equal? x y)
  (cond
    ; Ensure both arguments are numbers
    [(and (number? x) (number? y))
    (let ([difference (custom-subtract y x)])
      (if (negative? difference) #f #t))]
    [else (error "Arguments must be numbers for comparison" x y)]))

; Custom implementation of greater-than-or-equal
(define (custom-greater-or-equal? x y)
  (cond
    ; Ensure both arguments are numbers
    [(and (number? x) (number? y))
    (let ([difference (custom-subtract x y)])
      (if (negative? difference) #f #t))]
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
