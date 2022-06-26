
# precondition

<!-- badges: start -->
[![R-CMD-check](https://github.com/tzakharko/precondition/workflows/R-CMD-check/badge.svg)](https://github.com/tzakharko/precondition/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/precondition)](https://CRAN.R-project.org/package=precondition)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Lightweight precondition, postcondition and sanity checks (aka. assertions) for R 

- `precondition("x is a scalar value", {{length(x)}} == 1, !is.na({{x}}))` 

  Check the condition and abort with an error printing the values of `length(x)` and `x` 
  if the check fails

- `postcondition("returns a large data frame", is.data.frame(.value.), nrow(.value) >= 100)` 

  Check the value returned by the function and abort with an error printing the returned value
  of the check fails

- `sanity_check("x is sorted", !is.unsorted({{x}}))`

  Check the condition and immediately terminate execution with an error if the check fails, 
  bypassing any installed error handlers


**Features**:

- simple usage
- opinionated predicates for precondition, postcondition and sanity check assertions
- type safety: assertions must evaluate exactly to `TRUE`, evaluation errors are silently 
  treated as `FALSE`
- detailed diagnostics of relevant values using the embrace operator `{{.}}`
- low overhead, can be used in performance-critical loops 
- no hard dependencies on other packages (will use other popular packages such as `rlang`, 
  `cli` and `pillar` to produce better formatted error messages)

## Installation

The latest development version of the package is available from GitHub and 
can be installed from the development repository

```r
pak::pak("tzakharko/precondition")
```

## Usage

Check a precondition like this:

```r
x <- -5
precondition("x must be positive", x > 0)
# Error:
# ! x must be positive
# ! `x > 0` is not TRUE
```

Show diagnostics on values of importance by wrapping them in the embrace to more 
easily see what went wrong:

```r
x <- 15
precondition(is.integer({{x}}), {{x}} > 10)
# Error:
# ! precondition failure
# ! `is.integer(x)` is not TRUE
#  
#  `x` = dbl 10

precondition(is.data.frame({{iris}}), {{nrow(iris)}} > 200)
# ! precondition failure
# ! `nrow(iris) > 200` is not TRUE
#   
#   `nrow(iris)` = int 150L
``` 

Check that the function produced a well-formed value using a postcondition check

```r
fun <- function(x) {
  postcondition(.value. > 0)

  x - 10
}

fun(5)
# Error in `fun()`:
# ! postcondition failure
# ! `.value. > 0` is not TRUE
#   
#   `.value.` = dbl -5
```

Verify a critical assumption that the logic of your code relies upon

```r
x <- produce_some_data_frame()

sanity_check(is.data.frame({{x}}))
# Fatal error:
# ! assertion failure
# ! `is.data.frame(x)` is not TRUE
#   
#   `x` = NULL
# 
# ℹ Failed an internal sanity check
# ℹ Please consider submitting a bug report
# 
# ! Unable to continue, terminating
```

## Detailed overview

Programs almost always rely on implicit assumptions, randing from simple ones like "this function 
argument is a non-empty string" to more involved ones like "my algorithm should produce a sorted 
vector". If these assumptions are broken, either due to a bug in the code or some unexpected 
input, the program will not behave as expected. Sometimes this will result in a runtime error 
message, but quite often the execution will continue, producing nonsensical values and behavior 
This can lead to frustrating and hard to track down issues, especially in a flexible, permissive 
programming environment like R.  

Potential issues of this kind can be caught early if the assumptions are explicitly declared and 
checked before the code that relies on them is executed. This is done by strategically placing 
assertions in the program. Assertions are predicates which ensure that the program fails visibly 
rather than continue executing incorrectly, while also showing the exact location of the error. 

This package implements a family of assertion functions to facilitate assumption validation
and debugging:

 - preconditions (via `precondition()`) are assertions that verify invariants required for 
   the program to continue executing correctly. This usually has to do with with proper usage of
   functions and interfaces such as checking that correct function arguments have been supplied
   or that the mandatory configuration file exists.

 - postconditions (via `postcondition()`) are assertions that verify invariants that must hold 
   true when a function successfully returns (this can be used to check the function return 
   value or the side effect that the function has produced)

 - sanity checks (via `sanity_check()`) are assertions that verify important internal code 
   invariants, for instance that a sequence of certain processing steps will always produce
   a data frame with a given set of columns. Sanity checks can be used to guard against possible 
   critical bugs in the code that would otherwise be difficult to detect. Failing a sanity check 
   is a fatal error from which recovery is impossible, since it means that the program itself is
   fundamentally flawed. Sanity check errors bypass the error handling mechanism of R and will 
   always result in program termination. 

The predicates `precondition()` and `sanity_check()` are stand-in replacement for `stopifnot()`, 
and `postcondition()` is a stand-in replacement for `on.exit(stopifnot())`. The principal 
difference between the first two predicates is that `precondition()` is intended to test
the *contract* (e.g. whether a function or a code segment is used correctly by a caller),
where `sanity_check` is intended to test the *internal logic of a program* (e.g. that an 
algorithm does what it is supposed to do for any acceptable input)[^1]. Precondition failures are 
regular R errors that can be processed in an error handler, sanity check failures are critical 
errors that will always result in an emergency termination of the program. 

[^1]: The distinction between preconditions and sanity checks was inspired by the Swift 
programming language, as motivated by Swift designers: 
https://lists.swift.org/pipermail/swift-evolution/Week-of-Mon-20151214/002389.html


 ## Alternatives

 - Base R offers [`stopifnot()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/stopifnot.html) for assertion checking.

 - The package [`assertthat`](https://cran.r-project.org/web/packages/assertthat/index.html) is a comprehensive framework for building assertion predicates that produce rich custom 
 diagnostics. `precondition` follows a different design philosophy that focuses on 
 simplicity and low runtime overhead. 
 
