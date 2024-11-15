 (test-case "Let Binding Shadowing"
    (check-equal? (startEval '(let ((x 5)) (let ((x 10)) (+ x 3))) '()) 13)
    (check-equal? (startEval '(let ((x 7)) (let ((x (* x 2))) (- x 3))) '()) 11))