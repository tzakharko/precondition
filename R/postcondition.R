#' @rdname precondition
#' @export
postcondition <- function(..., .env = parent.frame()) {
  error <- FALSE

  # the postcondition handler
  postcondition <- function() {
    # inject a pronoun for the returned value that can be used in the postconditions
    embraced_exprs <- list()
    delayedAssign(".value.", {
      embraced_exprs <- list(quote(.value.))
      returnValue()
    }, assign.env = .env)
  
    # check the postconditions
    if(.External2(ffi_check_conditions)) return()

    # only diagnose if the function exited correctly
    if(isTRUE(.shared$suppress_postcondition_diagnostics)) return()

    # report the error
    error_msg <- c(
      extract_assertion_message(..., default = "postcondition failure"),
      diagnose_failed_conditions(.env, embraced_exprs, ...)
    )

    # install and execute the error call (this will create cleaner trace)
    error_call <- make_error_call(error_msg, .env)
    .error <- NULL
    eval(bquote(delayedAssign(".error", .(error_call))))
    .error
  }

  # clear the diagnostics suppression
  .shared$suppress_postcondition_diagnostics <- FALSE

  # install the postcondition handler
  do.call(on.exit, list(as.call(list(postcondition)), TRUE), envir = .env)
}
