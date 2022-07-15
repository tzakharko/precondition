#' Assertion predicates
#'
#' @description
#'
#' The assertion predicates described here are similar in function to base R
#' [base::stopifnot()], but offer improved diagnostics and safer behavior.
#'
#' - `precondition()` will immediately evaluate it's arguments and fail if
#'   any of them do not evaluate to `TRUE`. Use this predicate to check
#'   code contracts such as validating function inputs or other state required
#'   to proceed correctly.
#'
#' - `postcondition()` will evaluate it's arguments *after* the caller function
#'   successfully returns, at which point it will fail if any of them
#'   do not evaluate to `TRUE`. Special pronoun `.value.` can be used in the
#'   assertion expression to refer to the value returned by the caller. Use this
#'   predicate to check that your function will have produced well-formed result
#'   or state.
#'
#' - `sanity_check()` will immediately evaluate it's arguments and fail if any
#'   of them do not evaluate to `TRUE`. This failure will result in immediate
#'   program termination and cannot be processed with the usual R error handling
#'   mechanisms. Use this predicate to guard your code agains potential bugs.
#'   Failing a sanity check means that the program contains a serious error and
#'   cannot continue execution.
#'
#' 
#' To aid debugging, you can wrap principal values inside assertions using a pair
#' of braces (e.g. `{{x}} > 0`). On assertion failure, values embraced in this way
#' will be printed out in the diagnostic message, making it easier to track the
#' problem. 
#'
#' Under certain circumstances these predicates might evaluate their arguments
#' more then once. Be wary of combining them with side effects.
#'
#' @param ... one or more assertion expressions to check. Each of these should
#'            evaluate to scalar `TRUE`. String literals can be used to provide
#'            informative messages to show on failure (see examples). 
#'
#' @param .env the call frame of the function where the assertion check is performed,
#'             for advanced use only. 
#'             
#'
#' @details
#'
#' - A precondition is an assertion that specifies a set of conditions which must be
#'   true in order for the execution to proceed in a meaningful way. This is usually
#'   conditioned on the user input or environment in some way. A postcondition is an
#'   assertion that must be true if a function has executed correctly. Pre- and
#'   postconditions should be used to check whether a function correctly interfaces
#'   with the rest of the program.
#'
#' - A sanity check (invariant check) is an assertion that specifies a set of conditions
#'   which the program inplicitly assumes to be true. A sanity check failure means
#'   that the core logic of the program is faulty and that error recovery is either
#'   not possible or not meaningful. Sanity checks should be used to test the internal
#'   logic of your code and guard agains potential bugs. A sanify check failure will 
#'   result in the immediate program termination (via [fatal_error]).
#'
#' - Arguments of assertion predicates can be string literals (constants) that provide
#'   an informative message, e.g. `sanity_check("x is not NULL", !is.null(x))`. This
#'   message will be shown in case of an assertion failure. If multiple such mesages
#'   are provided, the last oen preceding the failed contition will be shown. 
#' 
#'   Note that the message *must* be a literal, the following will not work: 
#'   `sanity_check(paste0("x is not", "NULL"), !is.null(x)). 
#'
#' - Values inside an assertion expression can be embraced using the `{{` operator,
#'   which will cause for them to be printed out on assertion error. For example,
#'   `precondition({{x}} > 0)` will print the value of `x` on assertion failure.
#'   The idea of `{{` operator is borrowed from tidyverse where it is used to forward
#'   arguments from one function to another without breaking the custom evaluation
#'   mechanism that tidyverse packages rely on (see [rlang::embrace-operator] for
#'   details). We extend this operator to mean forwarding in more generic sense,
#'   as in "take the value of the embraced value here". This gives us an ergonomic way to
#'   empathize important values while retaining conceptual compatibility with popular
#'   frameworks. Note that the assertion predicates in `precondition` package themselves
#'   do not perform any non-standard evaluaton. 
#'
#' - `postcondition(check)` is equivalent to `on.exit(stopifnot(check))`, except that the
#'   postcondition will not report an error if there already was an error during function
#'   execution.
#'
#' @examples
#'
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
  error_msg <- diagnose_failed_conditions("precondition failure", .env, list(), ...)

  # install and execute the error call (this will create cleaner trace)
  error_call <- make_error_call(error_msg, .env)
  .error <- NULL
  eval(bquote(delayedAssign(".error", .(error_call))))
  .error
}
