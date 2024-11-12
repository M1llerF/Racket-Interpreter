# Custom Racket Interpreter Documentation

## 1. Promises, Delay, and Force
`promises`: A promise represents an expression that hasn't been evaluated yet but will be evaluated later when needed.
In order to use `promises`, we need to implement to functions: 

- `delay`: Creates a promise which will be evaluated when required.
- `force`: Forces evaluation of a delayed expression. So when a result is needed force will evaluate it and return the result.

### Example 1: Using Code:
In our custom interpreter, the handling of `letrec` expressions invovles using promises to allow for recursive defintions. 
For example we could have: 
```
(letrec ([x (+ 1 y)]
         [y (* 2 x])
  (+ x y))
  ```
First let's see what happens if we do not use `delay` for `x` and `y`:
1. The interpreter tries to evaluate `x`, which is defined as `(+ 1 y)`. Since `y` has not been assigned yet, it then tries to evaluate `y`.
2. `y` is defined as `(* 2 x)`, which depends on the value of `x`. But `x` hasn't been assigned because it needed to find out the value of `y`.
3. So we go back to step 1., then step 2., then 1. 2. 1. 2., indefintely since neither value is fully assigned (because their evaluation depends on eachother).

**Let's look at this with promises, delays, and force**
1. **Promise creation**:

   - The interpreter encounters `letrec` and sees two variables: `x` and `y`.
   - It does **not** evaluate `(+ 1 y)` or `(* 2 x)` immediately. Instead, it creates **promises** for both.

2. **Promises in Environment:**

   - At this stage both `x` and `y` are in the environment, but they are not fully evaluated - they are **promises**
   - This is **really, really important** since it means that when we look up `x` and `y`, we find a valid reference (the promise), and not a undefined value.
3. **Forcing the Promises in the Body:**

   1.  **Force** `x`:
      - When evaluating `(+ x y)`, the interpreter **forces** the promise for `x`.
      - Forcing `x` means that the interpreter now evaluates the expression associated with the promise, which is `(+ 1 y)`.
   2. **Evaluating** `(+ 1 y)`
      - To evaluate `(+ 1 y)`, the interpreter needs the value of `y`, which it looks up `y` it finds a `promise`.
        The interpreter then **forces** the promise.
   3. **Force** `y`:
      - In order to force `y` we need to evaluate `(* 2 x)`.
      - !!Imporantly!!, by this point the environment already has `x` as a promise that has started evaluating. This means that the interpreter can look up `x` and continue working without causing a circular evaluation issue.

The key difference is that **force** introduces **incremental evaluation**:
- When `x` is forced, it starts its evaluation while holding a reference to `y` in its promise form.
- When `y` is forced in the process of evaluating x, the interpreter can see that there is a valid reference to `x` (even though it is not yet fully evaluated).
- This means the interpreter can evaluate each part of the promise just enough to proceed step-by-step through the dependency chain.
- By deferring until all bindings are present, letrec allows each variable to at least exist in some form before being fully evaluated, enabling the interpreter to proceed without getting trapped in an infinite cycle.

### Example 2: Using an Analogy:
Think about cooking a complex dish where two of the ingredients depend on each other:
-**Ingredient A** requires **Ingredient B** to be partially prepared before you can finish it.
- Similarly, **Ingredient B** also requires **Ingredient A** to be partially prepared.

If you try to prepare both immediately and completely without waiting for each other, you'll get stuck because they each depend on each other being fully ready.

Instead, you need to defer certain parts of the preparation:
- You prepare **Ingredient A** up to the point where you need **Ingredient B**.
- You do the same for **Ingredient B.**
- At that point, both are partially prepared and have enough information to be finished together.

## 2. Handling Let and Letrec
So both `let` and `letrec` introduce new variables into a local scope, but hey work differently when it comes to recursion: 
- `let`: Binds each variable to a value and evalautes the body of the expression suing those bindings. Variables defined in `let` cannot refer to each other.
- `letrec`: Used in recursive bindings where variables need to refer to themselves or to each other. This has been explained above.
