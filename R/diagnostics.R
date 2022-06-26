# Perform diagnostics on failed conditions and produce a pretty-printed summary
#
# The returned diagnostics specify the failed condition as well as the values 
# for the embraced expressions (if any) to facilitate debugging
diagnose_failed_conditions <- function(.env, embraced_exprs, ...) {
  # -- a precondition failed, need detailed diagnostics for each condition
  conditions <- as.list(substitute(list(...)))[-1]

  idx <- seq_len(...length())

  # skip the assertion message
  if(is_string_literal(conditions[[1]])) idx <- idx[-1L]

  for(i in idx) {
    if(suppressWarnings(check_conditions(...elt(i)))) next

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

    # return the diagnostics
    diagnostics <- c(
      "!" = paste(format_expr(condition_expr), "is not TRUE"),
      details,
      ""
    )

    return(diagnostics)
  }
}

is_string_literal <- function(x) {
  !is.object(x) && is.character(x) && length(x) == 1L && !is.na(x)
}

extract_assertion_message <- function(msg, ..., default = "") {
  if(missing(msg)) return(default)

  x <- substitute(msg)
  # check if this is a string literal
  if(is_string_literal(msg)) {
    msg
  } else {
    default
  }
}
