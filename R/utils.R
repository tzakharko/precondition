unsafe_data_frame <- function(...) {
  out <- list(...)
  class(out) <- "data.frame"
  rownames(out) <- if(length(out) > 0L) seq_along(out[[1L]]) else integer()

  out
}

delayedAssign0 <- function(name, expr, eval_env = parent.frame(), assign_env = parent.frame()) {
  # force the parameter evaluation
  assign_env <- assign_env
  eval_env <- eval_env
  
  # execute delayedAssign with expression substituted
  eval(bquote(delayedAssign(name, .(expr), eval.env = eval_env, assign.env = assign_env)))
}

get_package_name <- function(env) {
  env <- topenv(env)
  if(isNamespace(env)) {
    getNamespaceName(env)
  } else {
    ""
  }
}