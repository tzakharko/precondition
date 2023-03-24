#' Signal a fatal error (and optionally terminate the program execution)
#'
#' @description
#'
#' `fatal_error()` is equivalent to the base function [base::stop()],
#' except it is intended to signal critical errors where recovery
#' is impossible or unfeasible.
#'
#' Fatal errors are signaled via [rlang::abort()] with the class
#' `precondition/fatal_error`. The option `fatal_error_action` controls 
#' the behavior of the fatal errors.
#'
#' - `option(fatal_error_action = "inform")` will display a
#'    warning if a fatal error has been prevented from bubbling up to the #
#'    user(either via `tryCatch() or some other error handling mechanism). This
#'    is the default setting and will draw user's attention to a fatal error
#'    occurring. 
#'
#' - `option(fatal_error_action = "none")` will make fatal errors
#'   behave like regular R error conditions. Use this if your code contains custom
#'   logic for handling fatal errors. 
#'
#' - `option(fatal_error_action = 'terminate')` will immediately
#'   the program execution without saving the workspace or running finalizers
#'   when a fatal error occurs. 
#'
#' @param bullets a character vector containing the error message,
#'                can be formatted in the style of [rlang::format_error_bullets()]
#'
#' @param ...     reserved for future use
#'
#' @details
#'
#' `fatal_error()` is used in [sanity_check()] to report critical assertion failures.
#'
#' @aliases precondition_fatal_error_action
#' @export
fatal_error <- function(bullets, ...) {
  rlang::check_dots_empty()
  rlang::is_bare_character(bullets) || rlang::abort("`bullets` should be a character vector")
  error <- make_error(bullets, class = "precondition/fatal_error")

  # invoke the fatal error action
  action <- get_fatal_error_action()  
  if(!is.null(action)) {
    action(error)
  } else {
    # if no action is specified, make sure that the error is displayed even if handled
    is_handled <- TRUE
    .shared$muffle_fatal_error <- function() is_handled <<- FALSE

    on.exit({ 
      # remove the muffle hook
      .shared$muffle_fatal_error <- NULL

      # only display the error if it has been handled
      if(isTRUE(is_handled)) {
        # display the error and an informative message
        suffix <- rlang::format_error_bullets(fatal_error_handled_info)
        error$message <- paste0(error$message, "\n\n", suffix)
        writeLines(con = get_error_con(), format_rlang_error(error))
      }
    })
  }

  # if we are still here, throw this as a proper error
  suffix <- rlang::format_error_bullets(c("x" = "fatal error"))
  message <- paste0(error$message, "\n\n", suffix)

  rlang::abort(message, class = "precondition/fatal_error")
}


fatal_error_handled_info <- c(
  "!" = "an error raised by `fatal_error()` was handled in `tryCatch()`",
  "i" = "fatal errors indicate critical failures in the program",
  "i" = "and should typically abort the program execution",
  "",
  "i" = "use `option(fatal_error_action = \"none\")` to silence this message",
  "i" = "see ?precondition_fatal_error_action",
  ""
)

get_fatal_error_action <- function() {
  action <- getOption("fatal_error_action", "inform")
  
  if(identical(action, "inform")) {
    NULL
  } else
  if(identical(action, "none")) {
    function(error) {}
  } else
  if(identical(action, "terminate") || identical(action, "quit")) {
    function(error) {
      # display the error
      suffix <- rlang::format_error_bullets(c("x" = "fatal error, execution halted"))
      error$message <- paste0(error$message, "\n\n", suffix)

      writeLines(con = get_error_con(), format_rlang_error(error))

      # exit to top in interactive mode, quit with error in script mode
      if(interactive()) {
        evalq(return(), .GlobalEnv)
      } else {
        quit(save = "no", status = 2L, runLast = FALSE)
      }
    }
  } else {
    type <- if(rlang::is_string(action)) {
      sprintf("'%s'", action)
    } else {
      sprintf("<%s>", typeof(action))
    }

    rlang::warn(c(
      sprintf("invalid `options(fatal_error_action = %s)`", type),
      "i" = "supported values are 'inform', 'none', or 'terminate'",
      "i" = "see `?precondition_fatal_error_action` for more information"
    ))

    NULL
  }
}

format_rlang_error <- function(error) {
  backtrace_opt <- getOption("rlang_backtrace_on_error", "full")
  backtrace <- !rlang::is_string(backtrace_opt, "none")
  simplify <- if(rlang::is_string(backtrace_opt, "branch")) "branch" else "none"

  format(error, backtrace = backtrace, simplify = simplify)
}

get_error_con <- function() {
  if(identical(Sys.getenv("TESTTHAT"), "true")) stdout() else stderr()
}

make_error <- function(
  bullets, 
  ...,  
  frame = parent.frame(),
  call = parent.frame(),
  trace = rlang::trace_back(bottom = frame),
  class = NULL
) {
  # fix bullet names
  if(length(bullets) > 0L) {
    if(is.null(names(bullets))) names(bullets) <- rep("*", length(bullets))

    names(bullets)[[1L]] <- ""
  }

  rlang::error_cnd(
    class = class,
    message = rlang::format_error_bullets(bullets),
    .frame = frame,
    trace = trace,
    call = call
  )
}

stop_assertion_failure <- function(assertion) {
  bullets <- format_diagnostic_message(assertion, parent.frame()$.diagnostics)

  error <- make_error(bullets, class = "precondition/assertion_error")
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
