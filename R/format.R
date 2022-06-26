# Pretty print a value summary
#
# We rely on [pillar::type_sum()] if that package is installed, 
# and use a rudimentary type summary otherwise
format_value <- function(value, max_width = 40L) {
  # type summary
  type_sum <- if(has_pillar()) {
    pillar::type_sum(value)
  } else 
  if(!is.object(value)) {
    typeof(value)
  } else {
    class(value)[[1]]
  }

  # formatted value
  value_sum <- if(is.null(value)) {
    ""
  } else
  if(!is.object(value)) {
    format_expr(value, max_width = max_width)
  } else {
    # try toString...
    tryCatch({
      fmt <- toString(value)
      if(nchar(fmt) > max_width) {
        fmt <- paste0(substr(fmt, 1L, max_width), "...")
      }
      fmt
    }, error = function(cnd) {
      ""
    })
  }

  paste(type_sum, value_sum)
}

# Produce a short string that represents an expression or value
format_expr <- function(expr, max_width = 40L, backticks = TRUE) {
  out <- if(is.symbol(expr)) {
    as.character(expr)
  } else {
    out <- deparse(expr, width.cutoff = max_width)
    if(length(out) > 1L) out <- paste0(out[[1L]], "...")
    out
  }
  
  if(isTRUE(backticks) && is.language(expr) && !grepl("^`.*`$", out)) {
    out <- paste0("`", out, "`")
  }

  if(has_cli()) {
    out <- cli::style_bold(out)
  }

  out
}

# Produce a short strign that summarizes a call
format_call <- function(call) {
  fn <- call[[1]]
  if(is.symbol(fn)) {
    paste0(as.character(fn), "()")
  } else {
    "<function>()"
  }
}

# Produce a short string that summarizes an error
format_error_string <- function(cnd, max_width = 40L) {
  msg <- unlist(strsplit(conditionMessage(cnd), "\\s*\n\\s*"))[[1L]]
  if(nchar(msg) > max_width) {
    msg <- paste0("<error/%s>", class(cnd)[[1L]])
  }

  msg
}

# Format an error prefix 
format_error_prefix <- function(prefix, call) {
  if(has_cli()) {
    prefix <- cli::col_red(cli::style_bold(prefix))
  }

  # add the call
  if(is.call(call)) {
    paste0(prefix, " in ", format_call(call), ":")
  } else {
    paste0(prefix, ":")
  }
}

# Format error bullets
format_error_bullets <- function(msg) {
  stopifnot(length(msg) > 0L)

  if(is.null(names(msg))) names(msg) <- c("!", rep("*", length(msg) - 1L))
  if(names(msg)[[1L]] == "") names(msg)[[1L]] <- "!"

  # format
  msg <- if(has_cli()) {
    cli::format_bullets_raw(msg)
  } else 
  if(has_rlang()) {
    rlang::format_error_bullets(msg)
  } else {
    # cleanup the bullets so it doesn't look too weird
    names(msg) <- ifelse(names(msg) %in% c("!", "*", "", " "), names(msg), "")
    paste(ifelse(names(msg) == " ", " ", names(msg)), msg)
  }

  paste0(msg, collapse = "\n")
}

# Format error trace
format_error_trace <- function(trace) {
  trace <- map_chr(trace, format_call)
  trace <- paste0(" ", seq_along(trace), ". ", trace)

  out <- c("Traceback:", trace, "")
  names(out) <- c("", rep(" ", length(out) - 1L))

  out
}

