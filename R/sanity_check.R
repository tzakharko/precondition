#' @rdname precondition
#' @export
sanity_check <- function(..., .env = parent.frame()) {
  # check the preconditions and exit on success
  if(.External2(ffi_check_conditions)) return(invisible(TRUE))

  # report the error
  ctx <- if(isNamespace(topenv(.env))) {
    sprintf(" in package `%s`", getNamespaceName(topenv(.env)))
  } else {
    ""
  }

  error_msg <- c(
    extract_assertion_message(..., default = "assertion failure"),
    diagnose_failed_conditions(.env, list(), ...),
    "i" = sprintf("Failed an internal sanity check%s", ctx),
    "i" = "Please consider submitting a bug report"
  )

  fatal_error(error_msg, call = .env)
}
