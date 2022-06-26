#' Report a fatal error and terminate execution
#'
#' @description
#'
#' `fatal_error()` is similar to [base::stop], except it bypasses
#' the error handling mechanism and will always immediately terminate
#' the program. Use this function to report critical execution errors, 
#' where recovery is impossible or unfeasible. 
#'
#' @param message the character vector containing the error message, 
#'                can be formatted in the style of [rlang::format_error_bullets](
#'                https://rlang.r-lib.org/reference/format_error_bullets.html)
#'
#' @param call    either a function call or a call frame in which the error 
#'                has occured
#'
#' @details
#'
#' In the interactive mode, `fatal_error()` will exit to the interpreter prompt. 
#' In a script, it will terminate execution using [base::quit]
#'
#' `fatal_error()` is used by [sanity_check] to immediately terminate execution
#' when a critical assertion in program logic has failed.
#'
#' @export
fatal_error <- function(message, call = parent.frame()) {
  # check if the requested action is to rais an error instead of quitting
  action <- getOption("precondition.fatal_error.action", "terminate")
  if(identical(action, "error")) {
    error_call <- make_error_call(message, call)
    .error <- NULL
    eval(bquote(delayedAssign(".error", .(error_call))))
    .error
  }

  # check that the action option is correct
  if(!identical(action, "terminate")) {
    msg <- "option `precondition.fatal_error.action` must be either 'terminate' or 'error'"

    if(has_rlang()) rlang::warn(msg) else warning(msg)
  }

  # -- print the error message and terminate 

  # format the prefix
  prefix <- format_error_prefix("Fatal error", call)

  # format the message
  message <- format_error_bullets(message)

  # format the trace (skip when testing to avoid messing the tests)
  trace <- if(is_testing()) {
    ""
  } else
  if(has_rlang()) {
    c("Traceback", format(rlang::trace_back()))
  } else {
    trace <- format_error_bullets(format_error_trace(sys.calls()))
  }

  # format the footer
  footer <- format_error_bullets("Unable to continue, terminating")

  # put all of it together and display the message
  message <- c(prefix, message, "", trace, "", footer)
  message <- paste0(message, collapse = "\n")

  writeLines(message, con = if(is_testing()) stdout() else stderr())
  
  # abort execution
  if(interactive()) {
    evalq(return(), .GlobalEnv)
  } else 
  # don't want to crash the interpreter if testing
  if(is_testing()) {
    stop("terminating")
  } else {
    quit(save = "no", status = 2L, runLast = FALSE)
  }
}

make_error_call <- function(message, call) {
  if(has_rlang()) {
    bquote(rlang::abort(.(message), call = .(call)))
  } else {
    # get the trace
    trace <- format_error_trace(as.list(sys.calls())[-1L])
    message <- format_error_bullets(c(message, trace))

    bquote(stop(.(message), call. = FALSE))
  }
}