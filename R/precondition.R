#' Pre- and postcondition checking
#'
#' @description
#'
#' The assertion predicates described here are equivalent to the base R
#' function [base::stopifnot()], but offer better diagnostics and safer
#' behavior.
#'
#' - `precondition()` specifies an assertion expression and fails with
#'   a diagnostic error if it does not evaluate to `TRUE`. Use this function 
#'   to validate inputs such as function arguments against code invariants.
#'
#' - `postcondition()` specifies an assertion expression to be evaluated after 
#'   the calling successfully exits. The execution with fail with a diagnostic 
#'   error if the assertion does not evaluate to `TRUE`. A special pronoun `.value.`
#'   can be used in the assertion expression to validate the function return value.
#'   Use this predicate to check that the function has produced a well-formed result
#'   or behavior.
#'
#' - `sanity_check()` specifies an assertion expression and immediately terminates
#'   with a fatal error if it does not evaluate to `TRUE`. Use this predicate to
#'   validate critical internal assumptions your code relies upon. Failing a sanity 
#'   check means that your program contains a serious error and cannot reasonably 
#'   continue execution.
#' 
#' To facilitate debugging, principal parts of the assertion expression can be
#' embraced using a pair of braces (e.g. `{{x}} > 0`). On assertion failure, the values
#' marked in this way will be displayed in the diagnostic message, making it easier
#' to understand why the assertion has failed.
#'
#' Under certain circumstances these predicates might evaluate the assertion expression
#' multiple times. Beware of combining them with side effects.
#'
#' @param ... one or more assertion expression to check. The first argument can also 
#'            optionally be a string literal containing an informative assertion 
#'            message
#'
#' @param .env the call frame of the function being tested
#'
#' @details
#'
#' - A precondition is an assertion that specifies a set of conditions that must be true
#'   in order for the execution to proceed in a meaningful way. This is usually
#'   conditioned on the user input or environment in some way. A postcondition is an
#'   assertion that must be true if a function has executed in a meaningful way. Pre- and
#'   postconditions explicitly state the contract of a function and make it easier to
#'   debug correct function usage. 
#'
#' - A sanity check is an assertion that specifies a set of conditions that the program
#'   implicitly assumes to be true. A sanity check failure means that the core logic
#'   of the program is broken and error recovery is either impossible or not meaningful.
#'   Sanity checks are used to test the internal logic of your code and will result 
#'   in an immediate program termination if failed (via [fatal_error]).
#'
#' - The first argument of any assertion predicate can be a string literal constant
#'   with an informative message, e.g. `sanity_check("x is not NULL", !is.null(x))`. 
#'   Note that this message *must* be a string literal, you cannot compute it or 
#'   use a variable. The following will not work correctly: 
#'   `sanity_check(paste0("x is not", "NULL"), !is.null(x)).
#'
#' - Values inside the assertion expression can be embraced using the `{{` operator, 
#'   which will cause for them to be displayed on assertion error. E.g. 
#'   `precondition({{x}} > 0)` will print the value of `x` on precondition failure. 
#'   The idea of `{{` operator is borrowed from tidyverse where it is used to forward
#'   arguments from one function to another without breaking the custom evaluation 
#'   mechanism that tidyverse packages rely on (see [rlang::embrace-operator] for 
#'   details). We extend this operator to mean forwarding in more generic sense, 
#'   as in "take the value of the embraced value here". This gives us an ergonomic way to 
#'   empathize important values while retaining conceptual compatibility with popular 
#'   frameworks. 
#'
#' - `postcondition(check)` is equivalent to `on.exit(stopifnot(check))`, except that the
#' postcondition will not report an error if there already was an error during function
#' execution.
#'
#' @examples
#' # These examples are guarded to avoid throwing errors
#' if (FALSE) {
#'
#' # function contract is accepting a positive value and returning up to 20
#' fun <- function(x) { 
#'   precondition({{x}} > 0)
#'   postcondition(.value. <= 20)
#'
#'   out <- x*2
#'   sanity_check("twice the x is larger than x", {{out}} > {{x}})
#'
#'   out
#' }
#'
#' fun(5)
#' fun(0)
#' fun(10)
#'
#' }
#' @export
precondition <- function(..., .env = parent.frame()) {
  # check the preconditions and exit on success
  if(.External2(ffi_check_conditions)) return(invisible(TRUE))

  # report the error
  error_msg <- c(
    extract_assertion_message(..., default = "precondition failure"),
    diagnose_failed_conditions(.env, list(), ...)
  )

  # install and execute the error call (this will create cleaner trace)
  error_call <- make_error_call(error_msg, .env)
  .error <- NULL
  eval(bquote(delayedAssign(".error", .(error_call))))
  .error
}
