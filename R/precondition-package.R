#' @useDynLib precondition, .registration = TRUE
NULL


# optional dependencies check
has_rlang <- function() requireNamespace("rlang", quietly = TRUE)
has_cli <- function() requireNamespace("cli", quietly = TRUE)
has_pillar <- function() requireNamespace("pillar", quietly = TRUE)


# [testthat::is_testing]
is_testing <- function() 
{
  identical(Sys.getenv("TESTTHAT"), "true")
}


# shared global state
.shared <- new.env(parent = baseenv())


.onLoad <- function(libname, pkgname){
  # install a global error callign handler that will suppress postcondition
  # diagnostics in the presence of an unhandled error
  #
  # one problem — globalCallingHandlers() cannot be called if there are 
  # conditon handlers present on stack and some package tools wrap 
  # this in tryCatch()
  # 
  # we thus first check whether there is tryCatch on the call stack
  # this is far from a robust solution
  #
  # ideally, on.exit() should have an additional argument that will stop
  # finalizers from executing when error is present
  for(i in seq_len(sys.nframe())) {
    if(identical(sys.function(i), tryCatch)) {
      # warning(paste0(
      #   "package `preconditions` is loaded inside a `tryCatch()` block",
      #   "unable to install condition hooks"
      # ), call. = FALSE, immediate. = TRUE)

      return()
    }
  }

  globalCallingHandlers(error = function(...) { 
    .shared$suppress_postcondition_diagnostics <- TRUE
  })
}