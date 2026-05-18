# Assignment 1—Formula Manipulator

## Introduction

In this assignment you will write a command-line interface program to
manipulate formulas containing variables, constant values, multiplications,
and additions. We have created a Haskell data type for these formulas, `Expr`,
that you have to use in this assignment. 

You can find the definition of `Expr` in the file
[`src/ExprLanguage.hs`](src/ExprLanguage.hs) together with the function
`parseExpr` to translate a String to an `Expr`.  In that source file you can
find [Haddock](https://haskell-haddock.readthedocs.io/latest/index.html)
documentation explaining how to use this module. 

**Tip** Run `cabal haddock` to generate nicely formatted HTML documentation.

## Assignment

This assignment consists of six programming exercises and a bonus. In the end you should
have a working program with a command-line interface as specified in Exercise
6.

In the six programming exercises you do not have to calculate a definition nor
give a definition in mathematical notation.  However, you have to explicitly
add a type declaration to every function you define in Haskell. As part of
these six programming exercises, you also have to write tests in
[`test/Spec.hs`](test/Spec.hs).

### Split over two weeks

The assignment is split into two parts, mainly so you have clear goals for the first week and can check your progress using Momotor output.

- In the first week, you focus on exercises 1 and 2, submit these for feedback from Momotor.

- In the second week, you finish the complete assignment. You submit your
  whole project.

### Grading

We grade you on Exercises 1-6 based on your submission in the second week.

#### Bonus
The bonus exercise is more challenging, but mandatory for an Excellent grade.

### Submission checklist

* Each function has a type signature, and is documented using [Haddock](https://www.haskell.org/haddock/); run `cabal haddock` to generate
and read your own documentation.
* Each function has at least two sensible tests in [`test/Spec.hs`](test/Spec.hs). For an excellent grade, more thorough testing is needed.
* `cabal build` works without compilation errors.
* `cabal test` passes.
* Names and student numbers are present in [`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) and [`test/Spec.hs`](test/Spec.hs)

### Getting started

1.  Clone this repository. It contains a Cabal project. Do not *manually* add
    files to or remove files from this project, nor change any file names. All
    you have to do is changing the contents of
    [`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) and
    [`test/Spec.hs`](test/Spec.hs).

2.  Verify that the project works by running `cabal test`. All tests pass.

3.  Do the exercises in this README. Invariant: All tests pass.

    "All tests pass" means that your project should build. **If your project
    does not build, you fail this assignment**. If you are unable to get
    something to work, comment out the problematic parts to keep the project
    working.

4.  In completing this assignment, you deliver two files:

    * The Haskell file [`src/FormulaManipulator.hs`](src/FormulaManipulator.hs)
      for Exercises 1 through 6.
    * The Haskell file [`test/Spec.hs`](test/Spec.hs) for Exercises 1 through
      6.

    Do not forget to have your names, student numbers, and date visible and at
    the top of all files you edit.

## Exercises

### Program the core functionality in module `FormulaManipulator`

#### Exercise 1

Just like we have implemented catamorphism factories `foldN` and `foldL` in
Haskell for the natural numbers and lists, we can also implement a
catamorphism factory for the `Expr` type.

Remember, the catamorphism factory `foldL` can be implemented in Haskell as
follows:

```haskell
foldL :: b -> (a -> b -> b) -> [a] -> b
foldL n c = rec
            where
              rec []      = n             -- nil  case
              rec (x:xs)  = c x (rec xs)  -- cons case
```

Analogous to `foldL`, implement the catamorphism factory for the type `Expr`
called `foldE` in [`src/FormulaManipulator.hs`](src/FormulaManipulator.hs).

**Hint**: There is a correspondence with a fold, the constructors of the data type and the identity function. For instance, the function `foldL [] (:)` is equivalent to the identity function for lists.
  
#### Exercise 2

Using the function `foldE`, implement function `printE` in
[`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) that pretty-prints an
expression. For example, an expression constructed like `Mult (Const 5) (Plus
(Var "x") (Const 3))` is pretty-printed as "(5 * (x + 3))".

For all expressions, it must hold that if you parse the pretty-printed
expression you get the original expression back. Stated in Haskell terms: for
all expressions `e` in `Expr` it holds `e == ((\(Right x) -> x) . parseExpr .
printE) e`

#### Exercise 3

Using `foldE`, implement an evaluator function `evalE` in
[`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) to calculate the
value of an expression. The type of this function is `(a -> Integer) -> (Expr
a Integer) -> Integer`.

The first parameter to `evalE` is a lookup table function that maps variables
to values. For example,

```{haskell}
evalE 
  (\v -> if v == "x" then 4 else error "unknown variable") 
  (Mult (Var "x") (Const 3))
```
  
should evaluate to `12`.

#### Exercise 4

Using `foldE`, implement function `simplifyE` in
[`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) that simplifies an
expression using the following simple rules:

* Handling units with multiplication and addition:
  
  * (∀ `x`: `Num x`: 0 + `x` = `x`)
  * (∀ `x`: `Num x`: 0 * `x` = 0)
  * (∀ `x`: `Num x`: 1 * `x` = `x`)

* Simplify constant expressions by evaluating them, for example

  * 3 * 15 can be simplified to 45 and
  * 7 + 12 can be simplified to 19

#### Exercise 5

Using `foldE`, implement function `diffE` in
[`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) to differentiate an
expression for a given variable. For example, `(printE . diffE "x") (Mult (Var
"x") (Const 2))`  should return `"((x * 0) + (1 * 2))"`.

Let `'` represent the derivative with respect to `x`. Implement the following
differentiation rules:

* Constant rule: (∀ `c`: `Num c` : `c'` = 0)
* `y'` = 0 if `y` ≠ `x`
* `x'` = 1
* Sum rule: (`f(x)` + `g(x))'` = `f'(x)` + `g'(x)`
* Product rule: (`f(x)` * `g(x)`)`'` = `f(x)` * `g'(x)` + `f'(x)` * `g(x)`
  
**Hint**  Using a catamorphism on `Expr` directly will not work. To apply the
product rule, you need access to both the original functions and their
derivatives.  There was a similar problem writing a function for `n!` as a
catamorphism (see Lecture 6). The solution was to use tupling of `(fac n, n)`,
for which we were able to use a catamorphism. This is called a *paramorphism*.
The same solution can be applied in this exercise: tuple `(Expr, Expr)`.

### Program a command-line interface

#### Exercise 6
  
Create a simple command-line interface. We already have written a suitable
`main :: IO ()` function in the file [`app/Main.hs`](app/Main.hs):

```haskell
main :: IO ()
main = do
        as <- getArgs
        putStrLn (processCLIArgs as)
```

This `main` function uses the IO monad. In the `main` function the
command-line arguments are read into the list `as` by the `getArgs` function.
This list with command-line arguments is then processed by the function
`processCLIArgs`. The output of `processCLIArgs` is printed to the console.

Implement this function `processCLIArgs :: [String] -> String` in
[`src/FormulaManipulator.hs`](src/FormulaManipulator.hs). You are allowed, but not required, to use the
following modules:

- [`Data.List.Split`](https://hackage.haskell.org/package/split-0.2.5/docs/Data-List-Split.html)
  to split lists based on some delimiter.
- [`Data.Either`](https://hackage.haskell.org/package/base-4.18.3.0/docs/Data-Either.html)
  for all kinds of `Either` related functionality.
- [`Text.Read`](https://hackage.haskell.org/package/base-4.18.3.0/docs/Text-Read.html) for parsing integers with `readMaybe` or `readEither`.

These modules are already imported in [`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) and added
to the Cabal project.

Compile and run your project as follows:

```bash
cabal build
cabal run formulator -- OPTION EXPR
```

Where `EXPR` is an expression that can be parsed with
the function `parseExpr` and `OPTION` is one of the following options:

* `--print` or `-p`: pretty-print the expression
* `--simplify` or `-s`: simplify **and** pretty-print the expression
* `--differentiate <VAR>` or `-d <VAR>`: differentiate
    expression for `<VAR>` **and** simplify **and** pretty-print the result
* `--evaluate <LOOKUP>` or `-e <LOOKUP>`: evaluate the
    expression given the `<LOOKUP>` table. The lookup table is a
    String containing a list of `<VAR>=<VALUE>` pairs
    separated by semicolons. For example, `"x=4;y=5"` should give
    `x` the value `4` and `y` the value
    `5`.
* `--help` or `-h` should give a short help
    message on how to use your program.
  
Your program should also give helpful error messages when things go wrong.

For example:

```bash
cabal run formulator -- --evaluate "f=1;g=7" "3 + 5 * g * f"   
  => "38"
cabal run formulator -- -d "f" "3 + 5 * 5 * f * f"             
  => "((25 * f) + (25 * f))"
cabal run formulator -- --simplify "3 + 1 * x + (x * 0) + 45"  
  => "((3 + x) + 45)"
cabal run formulator -- -p "3 + 1 * x + (x * 0) + 45"          
  => "(((3 + (1 * x)) + (x * 0)) + 45)"
```

## Bonus 
Follow the instruction on [School of Haskell](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras), but apply it to a similar expression language we have used so far. 

1. Define your own data type `ExprF` with appropriate constructors.
2. Define a [`Functor`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Functor.html) instance for this data type.
3. Define the function, `toExprF`. Using this in combination with `parseExpr` you should be able to get `Fix (ExprF String Integer)` from a string.
4. Using the above data type, and the `cata` function, implement the `normalizeE` function. The normalize function should normalize an expression such that semantically equivalent expressions are normalized to the same expression. For instance, `normalizeE` applied to `(5+x)` and `(x+5)` should return the same expression for both. Also consider more complex examples such as `(5*x)+(y*x)` and `(4*x)+(y*x)+x`. However, the normalized expression should always be equivalent to the non-normalized one.
**Note** You are free to choose how this normalized form should look. Additionally, you may use the packages loaded in the project file, such as `Data.Map` from the `containers` package. One solution is building up an alternative data structure using the `cata` and afterwards unfold that data structure to on expression again.
