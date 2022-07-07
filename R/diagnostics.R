# Perform diagnostics on failed conditions and produce a pretty-printed summary
#
# The returned diagnostics specify the failed condition as well as the values 
# for the embraced expressions (if any) to facilitate debugging
diagnose_failed_conditions <- function(default_message, .env, embraced_exprs, ...) {
  # -- a precondition failed, need detailed diagnostics for each condition
  conditions <- as.list(substitute(list(...)))[-1]

  idx <- seq_len(...length())
  assertion_message <- NULL

  for(i in idx) {
    # update the assertion message
    if(is_string_literal(conditions[[i]])) {
      assertion_message <- conditions[[i]]
      next
    }

    # check the condition
    check_error <- NULL
    check_result <- suppressWarnings(tryCatch(...elt(i), error = function(error) {
      check_error <<- error
      NULL
    }))
    if(identical(check_result, TRUE)) next

    # record and strip embraced subexpressions
    condition_expr <- deembrace(conditions[[i]], function(expr) {
      embraced_exprs <<- c(embraced_exprs, expr)
    })
    
    # format embraced expressions along with their values
    details <- map_chr(embraced_exprs, function(expr) {
      value <- tryCatch(
        paste0("= ", format_value(eval(expr, .env))), 
        error = function(cnd) {
          # check for missing arguments
          if(is.symbol(expr)) {
            check <- as.call(list(missing, expr))
            if(tryCatch(eval(check, .env), error = function(.) FALSE)) {
              return(" is missing, with no default")
            } 
          }

          # report the error
          sprintf("failed to evaluate (%s)", format_error_string(cnd))  
        }
      )

      paste(format_expr(expr), value)
    }) 

    # add appropriate spacing for nicer output 
    if(length(details) > 0L) details <- c("", details)
    names(details) <- rep(" ", length(details))

    # diagnose the check result
    additional_diagnostics <- if(!is.null(check_error)) {
      "note: error occured when evaluating the condition"
    } else 
    if(is.atomic(check_result) && length(check_result) > 0L && all(is.na(check_result))) {
      "note: condition should produce a scalar TRUE or FALSE (NA found)"
    } else 
    if(!is.object(check_result) && is.logical(check_result) && length(check_result) > 1L) {
      "note: condition should produce a scalar TRUE or FALSE (logical vector found)"
    } else 
    if(!identical(check_result, FALSE)) {
      "note: condition should produce a scalar TRUE or FALSE"
    } else {
      character(0)
    }

    # return the diagnostics
    if(is.null(assertion_message) || grepl("^\\s*$", assertion_message)) {
      assertion_message <- default_message
    }
    diagnostics <- c(
      assertion_message,
      " " = paste(format_expr(condition_expr), "is not TRUE"),
      details,
      "",
      "i" = additional_diagnostics
    )

    return(diagnostics)
  }
}

is_string_literal <- function(x) {
  !is.object(x) && is.character(x) && length(x) == 1L && !is.na(x)
}
