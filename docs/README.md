# Racket Interpreter for a Subset of Racket

## Overview
This project implements an interpreter in Racket for a subset of the Racket language itself. The primary function, `startEval`, serves as the entry point for evaluating expressions. The interpreter handles various Racket constructs including arithmetic operations, conditionals, lambdas, and list operations.

## Features
The interpreter supports the following constructs:
- **Constants and Variables**: Evaluation of numbers, symbols, and quoted expressions.
- **Arithmetic Operators**: `+`, `-`, `*`, `/` (binary operators only).
- **Relational Operators**: `equal?`, `=`, `<`, `>`, `<=`, `>=`.
- **List Operations**: `car`, `cdr`, `cons`, and `pair?`.
- **Conditionals**: The `if` construct.
- **Lambda Expressions**: Function definitions and closures.
- **Function Applications**: Invoking functions with arguments.
- **Local Bindings**: `let` and `letrec` constructs for variable scoping.

## Code Organization
The project consists of:
1. **Core Interpreter (`interpreter.rkt`)**: Implements the `startEval` function with evaluation rules for various constructs.
2. **Test Suites**:
   - `arithmetic-tests.rkt`: Tests for arithmetic operations.
   - `relational-tests.rkt`: Tests for relational operators.
   - `list-operations-tests.rkt`: Tests for list operations.
   - `if-statement-tests.rkt`: Tests for conditionals.
   - `lambda-tests.rkt`: Tests for lambda expressions and applications.
   - `local-binding-tests.rkt`: Tests for `let` and `letrec` bindings.
   - `interpreter-tests.rkt`: General tests covering constants, quoting, and recursive constructs.

## Key Data Structures
- **Environment**: The interpreter uses an environment represented as an association list (`alist`) to map symbols to their values.
- **Closures**: Lambda expressions are evaluated into closures, which encapsulate parameters, body, and the environment in which they were defined.
- **Promises**: Used to support `letrec` with delayed evaluation.

## Limitations
1. **No Optional or Keyword Arguments**: Lambda expressions only support positional arguments.
2. **Single Expression Bodies**: Constructs like `let` and `lambda` allow only one expression in their body.
3. **No Side Effects**: The interpreter does not support operations with side effects, adhering to functional programming paradigms.
4. **Division by Zero**: Explicitly handled with errors but without sophisticated error recovery.

## Testing
Comprehensive tests are provided for each construct. Key highlights:
- **Arithmetic**: Verifies support for BEDMAS and edge cases like division by zero.
- **Lambdas**: Tests include nested and higher-order functions.
- **Local Bindings**: Covers shadowing, recursive definitions, and interdependent bindings.
- **Conditionals**: Includes simple and nested `if` statements.
- **List Operations**: Tests `car`, `cdr`, `cons`, and pair predicates.

Sample outputs for recursive constructs:
- **Factorial (using `letrec`)**:
  ```racket
  (startEval '(letrec ((fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))) (fact 10)) '())
  ```
  Expected result: `3628800`.

## How to Run
1. Install Racket (version X or higher) and ensure the `rackunit` library is available.
2. Load the `interpreter.rkt` file into Racket to evaluate expressions:
   ```racket
   (require "interpreter.rkt")
   (startEval '(<expression>) <environment>)
   ```
3. To run all tests:
   ```sh
   racket all-tests.rkt
   ```
   This executes a unified test suite combining all individual tests.
4. To test specific features, run individual test files:
   ```sh
   racket arithmetic-tests.rkt
   racket list-operations-tests.rkt
   ```

## Dependencies
- **Racket**: Version X or higher.
- **rackunit**: For running test suites.

## Acknowledgments
This project was implemented as part of an academic course to explore interpreter design and functional programming principles. Special thanks to the instructor for the structured guidelines.

## License
All Rights Reserved

