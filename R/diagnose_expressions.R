#' Diagnose expressions and substitute debug markers
#'
#' @aliases debug-markers
#' @export
#'
#' @description
#'
#' Assertions in the `precondition` package support debug markers to provide
#' user-friendly assertion failure diagnosis. The low-level diagnostic
#' machinery is implemented by `diagnose_expressions()`. Advanced users can
#' make use of this function in their own code or when implementing custom
#' assertion helpers (see [diagnose_assertion_failure()]).
#'
#' Use single curly braces `{x}` to mark expressions of interest and make them
#' appear as separate entries in the diagnostic output. Use double curly braces
#' `{{x}}` to perform checks on behalf of a parent function and display
#' diagnostics in the context of the parent.
#'
#' @param ... expressions to diagnose
#' @param .env (advanced) the environment where the diagnosis should be performed
#' @returns a data frame with diagnostic information
#'
#' @details
#'
#' `diagnose_expressions()` supports two kinds of debug markers. Both rely on
#' wrapping expressions in one or more curly braces `{}`.
#'
#' - wrapping an expression in curly braces (e.g. `{x} > 0`) means that the this
#'   expression is of particular interest and should be diagnosed separately.
#'   The braces will be removed from the diagnostic output and the wrapped
#'   expression will be added as a separate entry in the diagnostic table
#'   (note: `diagnose_expressions({x} > 0)` is equivalent to `debug_expressions
#'   (x > 0, x)`).
#'
#' - wrapping a function argument in two curly braces (e.g. `{{arg}} > 0) means
#'   that the argument is being been forwarded from a parent function. This
#'   concept of forwarding is borrowed from tidyverse's
#'   [rlang::embrace-operator]. A forwarded argument will be replaced by the
#'   original caller expression in the diagnostic output.
#'
#' `diagnose_expressions()` returns a data frame with one row per diagnosed
#' expression(either supplied as an argument or marked via `{}`) and three
#' columns. The column `expr` is a list of diagnosed expressions, with debug
#' markers processed and substituted. The column `eval_result` is a list of
#' evaluated results for each diagnosed expressions. The column `is_error` is a
#' logical vector where value of `TRUE` indicates that an error occurred when
#' evaluating the respective expression. In this case the corresponding value
#' of `eval_result` will capture the error condition.
#'
#' Note that expressions or their parts might be evaluated more then once during
#' diagnosis. Side effects in diagnosed expressions can lead to unexpected
#' behavior.
#'
#' @examples
#'
#' x <- 10
#' diagnose_expressions({x} > 0, {x} > 15)
#'
#' helper <- function(arg) {
#'    cat(sprintf("`arg` is forwarded `%s`\n", forwarded_arg_label(arg)))
#'    diagnose_expressions({{{arg}}} > 0)
#' }
#' fun <- function(x) {
#'   helper(x)
#' }
#' fun(10)
diagnose_expressions <- function(..., .env) {
  # the base, not substituted, expressions
  exprs <- as.list(substitute(list(...)))[-1L]
  if(missing(.env)) .env <- sys.frame(sys.parent())

  # substitute the expressions
  substituted <- list() 

  i <- 1L
  while(i <= length(exprs)) {
    substituted[[i]] <- substitute_debug_expression(exprs[[i]], .env, function(expr) {
      exprs <<- unique(c(exprs, list(expr)))
      expr
    })
    i <- i + 1L
  }

  # evaluate the expressions
  results   <- vector("list", length(exprs))
  is_error <- logical(length(exprs))

  # evaluate all expressions
  for(i in seq_along(exprs)) {
    .eval <- NULL
    delayedAssign0(".eval", exprs[[i]], eval_env = .env)

    results[[i]] <- tryCatch(suppressWarnings(.eval), error = function(error) {
      is_error[[i]] <<- TRUE

      error$call <- substituted[[i]]
      error
    })
  }

  # return the table with the evaluated expressions
  unsafe_data_frame(
    expr = substituted,
    eval_result = results,
    is_error = is_error
  )
}




#' Perform substitution of debug markers and extract embraced expressions
#'
#' @noRd
substitute_debug_expression <- function(expr, env, embrace_callback = identity) {
  substitute_one <- function(expr, is_function = FALSE) {
    n_braces <- if(is_function) 0L else count_braces(expr)

    # {{ x }}
    if(n_braces == 2L) {
      expr <- substitute_forwarded_arg(expr, env)
    } else 
    # { x }
    if(n_braces == 1L || n_braces == 3L) {
      expr <- expr[[2]]

      # invoke the callback
      embrace_callback(expr)

      # substitute the embraced expression
      expr <- substitute_one(expr)
    } else if(is.pairlist(expr) || is.call(expr)) {
      is_fun <- rlang::is_call(expr, "function")

      for(i in seq_len(length(expr))) {
        expr[[i]] <- substitute_one(expr[[i]], is_function = is_fun)
      }
    } 

    expr
  } 

  substitute_one(expr)
}

#' Substitute {{ arg }} in the parent frame
#'
#' @noRd
substitute_forwarded_arg <- function(expr, env) {
  # get the function where substitution is performed
  # note: we can to wrap sys.parent() into a function call
  # since R messes up the frame stack when using eval
  iframe <- evalq((function() sys.parent())(), env)
 
  fun <- sys.function(iframe)
  frame <- sys.frame(iframe)

  arg <- expr[[2L]][[2L]]
  if(!is.symbol(arg) || !has_named_arg(fun, arg)) {
    rlang::warn(c(
      "double brace forwarding must be used with a function argument",
      sprintf("got `%s`", format_expr(expr))
    ), call = sys.call(iframe))
    return(expr)
  }

  # substitute the argument in the call frame
  arg <- eval(call("substitute", arg, frame))

  # if arg is not a symbol it has to be substituted
  if(!is.symbol(arg)) {
    iframe <- sys.parents()[iframe]
    frame <- sys.frame(iframe)
    
    arg <- substitute_debug_expression(arg, frame)
  }

  arg
}

#' Count the number of {{{ ... }}}
#'
#' @noRd
count_braces <- function(expr) {
  n <- 0L
  while(rlang::is_call(expr, "{", 1L)) {
    expr <- expr[[2L]]
    n <- n + 1L
  }

  n
}

#' Check if the function has an argument with a given name
#'
#' @noRd
has_named_arg <- function(fun, arg_name) {
  as.character(arg_name) %in% names(formals(fun))
}
