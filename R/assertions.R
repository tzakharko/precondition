#' Pre- and postcondition checking (assertions)
#'
#' @description
#'
#' The assertions described here are similar in functionality to the base R
#' function [base::stopifnot()], but focusing on better diagnostics, safer
#' behavior, and customizability.
#'
#' - `precondition()` fails with diagnosis if its arguments do not evaluate as
#'   `TRUE`. Use this assertion function to check function arguments or data
#'   inputs against code invariants.
#'
#' - `postcondition()` is as above, but the assertion is performed when the
#'   calling function successfully returns. Use this assertion to check that
#'   the function has produced a well-formed result (via [base::returnValue
#'   ()]) or behavior.
#'
#' - `sanity_check()` is as above, but the program execution will immediately
#'   terminate via [fatal_error()], bypassing R's error-checking mechanisms.
#'   Use this predicate to validate critical internal assumptions your code
#'   relies upon. Failing a sanity check means that your program contains an
#'   unrecoverable logical error and cannot reasonably continue execution.
#' 
#' To facilitate debugging, the assertions used with these functions can be
#' enhanced with debug markers. This enables informative error messages and
#' makes it easier to understand why the assertion has failed. First,
#' assertions can include custom informative messages, supplied via literal
#' string arguments to the assertion function. Second, key parts of the
#' assertion expression can be wrapped in curly braces(e.g. `{x} > 0`). If the
#' assertion fails, the values marked in such way will be diagnosed and
#' displayed as a separate entry in the error message. See the examples on how
#' to use these features and the details section how to implement even more
#' custom functionality.
#'
#' Under certain circumstances these predicates might evaluate the assertion expression
#' multiple times. Beware of combining them with side effects.
#'
#' @param ... one or more expressions to check (with optional assertion messages)
#' @returns `TRUE` on assertion success, raises an error of class 
#'          `precondition/assertion_error` on assertion failure
#'
#' @details
#'
#' A precondition is an assertion that specifies a set of conditions that must
#' be true in order for the execution to proceed in a meaningful way. This is
#' usually conditioned on the user input or environment in some way. A
#' postcondition is an assertion that must be true if a function has executed
#' in a meaningful way. Pre- and postconditions explicitly state the contract
#' of a function and make it easier to debug correct function usage. Note:
#' `postcondition(check)` is similar to `on.exit(stopifnot(check))`, except
#' that the postcondition will not be checked if an error occurred during
#' function execution. 
#'
#' A sanity check is an assertion that specifies a set of conditions that the
#' program implicitly assumes to be true. A sanity check failure means that the
#' core logic of the program is broken and error recovery is either impossible
#' or not meaningful. Sanity checks are used to test the internal logic of your
#' code and will result in an immediate program termination if failed (via
#' [fatal_error]).
#'
#' The arguments to these assertion functions are either expressions that should
#' evaluate to `TRUE` or literal string constants containing informative
#' messages (e.g. `sanity_check("x is not NULL", !is.null(x))`). Should the
#' assertion fail, the provided message will be displayed. Note that this
#' message *must* be a string literal, you cannot compute it or use a variable.
#' The following will not work correctly: `sanity_check(paste0("x is
#' not", "NULL"), !is.null(x)).
#'
#' Assertion expression support [debug-markers]. See [diagnose_assertion_failure()]
#' on how to implement custom assertion helpers. 
#'
#' @examples
#' # These examples are guarded to avoid throwing errors
#' if (FALSE) {
#'
#' # function contract is accepting a positive value and returning up to 20
#' fun <- function(x) { 
#'   precondition("`x` should be positive", {x} > 0)
#'   postcondition(returnValue() <= 20)
#'
#'   out <- x*2
#'   sanity_check("twice `x` is larger than `x`", {out} > {x})
#'
#'   out
#' }
#'
#' fun(5)
#' fun(0)
#' fun(10)
#' }
#' @export
precondition <- function(...) {
  .External2(ffi_assert_all) ||
    stop_assertion_failure("precondition failure")
}

#' @rdname precondition
#' @export
postcondition <- function(...) {
  # reset diagnostics suppression
  .shared$suppress_postcondition_diagnostics <- FALSE

  # install the postcondition hook
  hook <- call("{",
    call("<-", quote(postcondition), postcondition_hook),
    substitute(postcondition(...))
  )
  do.call(on.exit, list(hook, TRUE), envir = parent.frame())
}

# the actual postcondition check is performed here
postcondition_hook <- function(...) {
  .shared$suppress_postcondition_diagnostics ||
    .External2(ffi_assert_all) ||
    stop_assertion_failure("postcondition failure")
}


#' @rdname precondition
#' @export
sanity_check <- function(...) {
  .External2(ffi_assert_all) || {
    bullets <- format_diagnostic_message("sanity check failure", environment()$.diagnostics)

    # add a notice if the failure happened inside a package

    
    if((pkg <- get_package_name(parent.frame())) != "")  {
      bullets <- c(
        bullets, 
        "",
        "!" = sprintf("Failed an internal sanity check in package '%s'", pkg),
        "!" = "Please consider submitting a bug report"
      )
    } else {
      bullets <- c(
        bullets, 
        "",
        "!" = "Failed an internal sanity check"
      )      
    }

    fatal_error(bullets)

    FALSE
  }
}


aaa <- function() {
  sanity_check(0 > 5)
}

