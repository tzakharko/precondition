
# precondition

<!-- badges: start -->
[![R-CMD-check](https://github.com/tzakharko/precondition/workflows/R-CMD-check/badge.svg)](https://github.com/tzakharko/precondition/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/precondition)](https://CRAN.R-project.org/package=precondition)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Lightweight precondition, postcondition and sanity checks (aka. assertions) for R 

- `precondition("x is a scalar value", {length(x)} == 1, !is.na({x}))` 

  Check the condition and abort with an error printing the values of `length(x)` and `x` 
  if the check fails

- `postcondition("returns a large data frame", is.data.frame({returnValue()}), nrow({returnValue()}) >= 100)` 

  Check the value returned by the function and abort with an error printing the returned value
  summary

- `sanity_check("x is sorted", !is.unsorted({x}))`

  Check the condition and immediately terminate execution with an error if the check fails, 
  bypassing any installed error handlers


**Features**:

- opinionated predicates for precondition, postcondition and sanity check assertions
- runtime safety: assertions must evaluate exactly to `TRUE`, evaluation errors are silently 
  treated as `FALSE`
- detailed failure diagnosis via debug markers in assertions
- low runtime overhead
- extensible via custom utility assertion helpers

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
# Error in `stop_assertion_failure()`:
# ! precondition failure
# • x must be positive
#   
#   x > 0    logi FALSE
```

Show diagnostic information for selected values by wrapping them in `{}` to quickly see
what went wrong:

```r
x <- 15
precondition(is.integer({x}), {x} > 10)
# Error in `stop_assertion_failure()`:
# ! precondition failure
# • `is.integer(x)` is not TRUE
#   
#   is.integer(x)    logi FALSE
#   x                num 15

precondition(is.data.frame({iris}), {nrow(iris)} > 200)
# Error in `stop_assertion_failure()`:
# ! precondition failure
# • `nrow(iris) > 200` is not TRUE
#   
#   nrow(iris) > 200    logi FALSE
#   nrow(iris)          int 150
``` 

Check that the function produced a well-formed value using a postcondition check

```r
fun <- function(x) {
  postcondition(returnValue() > 0)

  x - 10
}

fun(5)
# Error in `stop_assertion_failure()`:
# ! postcondition failure
# • `returnValue() > 0` is not TRUE
#   
#   returnValue() > 0    logi FALSE
```

Verify a critical assumption that the logic of your code relies upon

```r
x <- produce_some_data_frame()

sanity_check(is.data.frame({x}))
# Error in `fatal_error()`:
# ! sanity check failure
# • `is.data.frame(x)` is not TRUE
#   
#   is.data.frame(x)    logi FALSE
#   x                   logi FALSE
# 
# 
# ℹ Failed an internal sanity check
# ℹ Please consider submitting a bug report
# 
# ✖ fatal error, terminating!
```

## Detailed overview

Programs almost always rely on implicit assumptions, from simple ones like "this function 
argument is a non-empty string", to more involved ones like "my algorithm should produce a sorted 
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

The predicates `precondition()` and `sanity_check()` are functional replacement for `stopifnot()`, 
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

## Assertion diagnosis and debugging

The assertion functions implemented in this package support special syntax to simplify problem
diagnosis and debugging. The basic tools are diagnostic messages and value diagnosis.

```r
             string literal with a custom message to be shown on failure
              |
             ~~~~~~~~~~~~~~~~~~~~  
precondition("x must be positive", {x} > 0)
                                   ^^^ 
                                     `x` is marked with braces, indicated that
                                     this value should be diagnosed separately
``` 

If the assertion fails, the last provided diagnostic message will be displayed. This allows 
one to chain multiple assertions with different messages, e.g. 

```r
precondition("`x` is integer", is.integer({x}), "`x` is larger than 10", {x} > 10)
```

Note that diagnostic messages must be string literals! Any non-literal expression that 
evaluates to a string will be interpreted as an assertion and will fail (since assertions
must evaluate to `TRUE`). 

The diagnostic machinery is implemented using the low-level function `diagnose_expressions()`
(see documentation). 

## Advanced customization and assertion helpers

The package provides means to customize the failure report via `diagnose_assertion_failure()`. 
This function can be used to implement custom assertion helpers and can be used by package 
developers to support the advanced functionality of `precondition`. Consider the following example.

```r
is_positive_int <- function(x) {
  # check that x is a positive integer
  is.integer(x) && length(x) == 1L && (x > 0) || {
    # report a custom assertion failure from the perspective of the caller function
    # `x` is substituted by whatever the caller uses 
    diagnose_assertion_failure(
      sprintf("`%s` must be a positive integer", forwarded_arg_label(x)),
      # double braces mean that the argument should be substituted 
      {{x}}
    )
  }
}

# for all intends and purposes this is just a regular R function that returns
# TRUE or FALSE
is_positive_int(5L)
# [1] TRUE
is_positive_int(-5L)
# [1] FALSE


# ... but it will provide custom diagnosis if invoked inside an assertion!
fun <- function(value) {
  precondition(is_positive_int(value))
}

fun(-5)
#Error in `stop_assertion_failure()`:
#! precondition failure
#• `value` must be a positive integer
#  
#  value    num -5
```

Note that the error message mentions the variable `value` instead of the variable `x` (
the actual variable we are checking inside `is_positive_int()`). This is done via an advanced
diagnostic marker `{{ }}`, which specifies that a function argument is being forwarded from
the parent function and should be replaced by parent's argument name in the diagnosis. This 
is functionally similar and follows the same syntactic convention as the 
[embrace operator](https://rlang.r-lib.org/reference/embrace-operator.html) in `tidyverse`.  

## Alternatives

 - Base R offers [`stopifnot()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/stopifnot.html) for assertion checking.

 - The package [`assertthat`](https://CRAN.R-project.org/package=assertthat) is a comprehensive framework for building assertion predicates that produce rich custom 
 diagnostics. `precondition` follows a different design philosophy that focuses on 
 simplicity and low runtime overhead. 
 
