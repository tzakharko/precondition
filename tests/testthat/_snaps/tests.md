# assertion message works in precondition()

    Code
      fun(10)
    Error <rlang_error>
      x must be a positive int
        `is.integer(x)` is not TRUE

---

    Code
      fun(0L)
    Error <rlang_error>
      x must be a positive int
        `x > 0L` is not TRUE

# assertion message works in postcondition()

    Code
      fun(0L)
    Error <rlang_error>
      postcondition failure
        `.value. > 0` is not TRUE
        
        `.value.` = int 0L

# assertion message works in sanity_check()

    Code
      fun(0L)
    Output
      Fatal error:
      ! x must be positive
        `x > 0` is not TRUE
      
      i Failed an internal sanity check in package `precondition`
      i Please consider submitting a bug report
      
      
      
      ! Unable to continue, terminating
    Error <simpleError>
      terminating

# embracing works in precondition()

    Code
      fun(10)
    Error <rlang_error>
      x must be a positive int
        `is.integer(x)` is not TRUE
        
        `x` = dbl 10

---

    Code
      fun(0L)
    Error <rlang_error>
      x must be a positive int
        `x > 0L` is not TRUE
        
        `x` = int 0L

# embracing works in sanity_check()

    Code
      fun(0L)
    Output
      Fatal error:
      ! x must be positive
        `x > 0` is not TRUE
        
        `x` = int 0L
      
      i Failed an internal sanity check in package `precondition`
      i Please consider submitting a bug report
      
      
      
      ! Unable to continue, terminating
    Error <simpleError>
      terminating

