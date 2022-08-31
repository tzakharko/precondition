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
#'                can be formatted in the style of [rlang::format_error_bullets]
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
fatal_error <- function(message) {
  error <- make_error(
    c(message, "", x = "fatal error, terminating!"),
    .class = "precondition/fatal_error"
  )

  # throw an error instead terminating if the respective option is set
  if(identical(getOption("precondition.fatal_error"), "error")) {
    rlang::cnd_signal(error)
  }

  # display the error
  writeLines(rlang::cnd_message(error, prefix = TRUE))

  # in interactive mode exit back to the prompt
  if(interactive()) {
    evalq(return(), .GlobalEnv)
  } else 
  # error our during testing
  if(is_testing()) {
    stop("fatal_error")
  } else {
    quit(save = "no", status = 2L, runLast = FALSE)
  }
}

make_error <- function(bullets, ...,  .class = NULL) {
  rlang::error_cnd(
    class = .class,
    message = rlang::format_error_bullets(bullets),
    trace = rlang::trace_back(bottom = parent.frame()),
    call = parent.frame()
  )
}

stop_assertion_failure <- function(assertion) {
  bullets <- format_diagnostic_message(assertion, parent.frame()$.diagnostics)

  error <- make_error(bullets, .class = "precondition/assertion_error")
  error$details <- parent.frame()$.diagnostics$details
  
  rlang::cnd_signal(error)
}


format_diagnostic_message <- function(prefix, info) {
  # debug details
  details <- format_diagnostic_details(info$details)
  if(length(details) > 0L) {
    details <- c("", details)
  }
  names(details) <- rep(" ", length(details))

  # format the message as bullets
  bullets <- info$message
  if(is.null(names(bullets))) {
    names(bullets) <- rep("*", length(bullets))
  }

  c(
    # prefix
    prefix,
    # message 
    bullets,
    details,
    # footer
    "",
    i = if(any(info$details$is_error)) {
      "note: error occured when evaluating the condition"
    } 
  )
}


format_diagnostic_details <- function(details) {
  if(is.null(details) || nrow(details) == 0L) return(character())

  # format the expressions and values
  exprs <- vapply(details$expr, format_expr, "")
  values <- vapply(details$eval_result, format_value, "")

  # format the expression table
  paste0(
    format(exprs, justify = "left"),
    "    ",
    values
  )
}
