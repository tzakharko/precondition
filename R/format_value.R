format_value <- function(x, max_width) {
  UseMethod("format_value")
}

#' @export
format_value.default <- function(x, max_width = 40L) {
  out <- utils::capture.output(utils::str(x, max.level=0L, width = max_width))[[1L]]
  gsub("(^\\s*)|(\\s*:?\\s*$)", "", out)
}

#' @export
format_value.call <- function(x, max_width = 40L) {
  format_value(as.expression(x), max_width =  max_width)
}


#' @export
format_value.NULL <- function(x, max_width = 40L) {
  "NULL"
}

#' @export
format_value.error <- function(x, max_width = 40L) {
  msg <- unlist(strsplit(conditionMessage(x), "\n"))[[1L]]
  msg <- gsub("(^\\s+)|(\\s+$)", "", msg)

  msg <- str_trim(msg, max_width)
  class <- class(x)[[1L]]
  if(class == "simpleError") class <- "error"

  sprintf("<%s: %s>", class, msg)
}


str_trim <- function(x, max_width) {
  if(nchar(x) > max_width) {
    x <- paste0(strtrim(x, max_width - 3L), "...")
  }
  
  x
}

format_expr <- function(expr, max_width = 40L) {
  lines <- deparse(expr, width.cutoff = max_width)
  out <- ""
  while(nchar(out) < max_width && length(lines) > 0L) {
    out <- paste0(out, gsub("(^\\s+)|(\\s+$)", "", lines[[1]]))
    lines <- lines[-1]
  }

  # add ellipsis
  if(nchar(out) >= max_width || length(lines) > 0L) {
    out <- paste0(substr(out, 1L, min(nchar(out), max_width - 3L)), "...")
  }

  out
}
