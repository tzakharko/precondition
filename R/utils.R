# A safer version of all 
#
# Checks that all arguments are TRUE, silencing errors and warnings
check_conditions <- function(...) {
  .External2(ffi_check_conditions)
}



# [purr::map_chr]
map_chr <- function(x, fun) {
  vapply(x, fun, "")
}


# locate the call associated with an environment
find_call <- function(call_or_env) {
  if(is.call(call_or_env)) return(call_or_env)

  if(is.environment(call_or_env)) {
    i <- sys.nframe() - 1L
    while(i > 0L) {
      if(identical(sys.frame(i), call_or_env)) {
        return(sys.call(i))
      }

      i <- i - 1L
    }
  }

  NULL
}

