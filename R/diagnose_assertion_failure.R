#' Implement a custom assertion
#' @export
#'
#' @description
#'
#' `diagnose_assertion_failure()` displays customized failure message and
#' diagnosis in assertions such as [precondition()]. This can be used to
#' implement assertion helpers. This function does nothing if invoked outside
#' an assertion (see details). The function `forwarded_arg_label()` looks up a
#' forwarded argument and formats it as a string (used in custom diagnostic
#' messages).
#'
#' @param message  diagnostic message to show (see [rlang::format_error_bullets()])
#' @param ...      expressions to diagnose (forwarded to [diagnose_expressions()])
#' @param .details an optional data frame with diagnosis data
#' @param arg      a forwarded function argument
#' @returns `diagnose_assertion_failure()` always returns `FALSE`. 
#'
#' @details
#'
#' If invoked as part of an assertion (e.g. [precondition()]), 
#' `diagnose_assertion_failure()` provides a custom failure message and
#' diagnosis. If invoked in any other context, the function does nothing. This
#' can be used to implement custom assertions helpers that behave like regular
#' binary predicates (functions) under normal circumstances and generate a
#' customized assertion failure report when used as part of an assertion
#' (see examples).
#'
#' The first argument to `diagnose_assertion_failure()` is a character vector
#' with a custom failure message. This vector will be formatted as error
#' bullets via[rlang::format_error_bullets()]. Any subsequent argument will be
#' forwarded to `diagnose_assertion_failure()` for diagnosis. For custom
#' diagnosis, the user can supply their own data frame with diagnosis details
#' via optional argument `.details`. The format of this data frame must be
#' identical to one returned by `diagnose_assertion_failure()`.
#'
#' The function `forwarded_arg_label()` looks up a forwarded expression and
#' formats it as a single string suitable for inclusion in diagnostic
#' messages.
#'
#' @examples
#'
#' # returns TRUE if x is a positive, integer, FALSE otherwise
#' # if invoked as part of an assertion displays a custom failure diagnosis
#' is_positive_int <- function(x) {
#'   is.integer(x) && length(x) == 1L && (x > 0) || {
#'     diagnose_assertion_failure(
#'       sprintf("`%s` must be a positive integer", forwarded_arg_label(x)),
#'       {{x}}
#'     )
#'   }
#' }
#' 
#' # for all intends and purposes this is just a regular R function that returns
#' # TRUE or FALSE
#' is_positive_int(5L)
#' is_positive_int(-5L)
#' 
#' # guard to avoid throwing errors
#' if(FALSE) {
#' 
#' # ... but it will provide custom diagnosis if invoked inside an assertion
#' precondition(is_positive_int(-5L))
#' 
#' }
diagnose_assertion_failure <- function(message, ..., .details) {
  if(!is.null(context_frame <- get_context_frame())) {
    if(missing(.details)) {
      .details <- diagnose_expressions(..., .env = parent.frame())
    }

    context_frame$.diagnostics <- list(
      message = message,
      details = .details
    )
  }

  FALSE
} 



#' @rdname diagnose_assertion_failure
#' @export
forwarded_arg_label <- function(arg) {
  arg <- call("{", call("{", substitute(arg)))
  expr <- substitute_forwarded_arg(arg, parent.frame())
  format_expr(expr)
}

get_context_frame <- function() {
  .Call(ffi_get_context_frame)
}

#' Default diagnosis message (invoked from the C routine)
#'
#' @noRd
make_default_message <- function(message, details) {
  if(is.null(message)) {
    suffix <- if(!isFALSE(details$is_error[[1L]])) {
      "has produced an error"
    } else {
      "is not TRUE"
    }

    message <- sprintf("`%s` %s", format_expr(details$expr[[1]]), suffix)
  }

  message
}
