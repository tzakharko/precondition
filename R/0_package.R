#' @useDynLib precondition, .registration = TRUE
NULL

.shared <- new.env(parent = emptyenv())

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
  # finalizers from executing when an error is present
  for(i in seq_len(sys.nframe())) {
    if(identical(sys.function(i), tryCatch)) {
      # rlang::warn(c(
      #   "!" = "package precondition is loaded inside a `tryCatch()` block",
      #   "!" = "unable to install condition hooks"
      # ))

      return()
    }
  }

  globalCallingHandlers(error = function(cnd) { 
    # notify that the fatal error bubbles up
    if(inherits(cnd, "precondition/fatal_error")) {
      if(!is.null(muffle <- .shared$muffle_fatal_error)) muffle()
    }

    .shared$suppress_postcondition_diagnostics <- TRUE
  })
}