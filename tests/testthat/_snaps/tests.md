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

# multiple assertion messages work in precondition()

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

---

    Code
      fun(3L)
    Error <rlang_error>
      x must be between 5 and 10
        `x >= 5` is not TRUE

---

    Code
      fun(11L)
    Error <rlang_error>
      x must be between 5 and 10
        `x <= 10` is not TRUE

# assertion message works in postcondition()

    Code
      fun(0L)
    Error <rlang_error>
      return value must be positive
        `.value. > 0` is not TRUE
        
        `.value.` = int 0L

# multiple assertion messages work in postcondition()

    Code
      fun(0)
    Error <rlang_error>
      return value must be positive
        `.value. > 0` is not TRUE
        
        `.value.` = dbl 0

---

    Code
      fun(3)
    Error <rlang_error>
      return value must be between 5 and 10
        `.value. >= 5` is not TRUE
        
        `.value.` = dbl 3

---

    Code
      fun(11)
    Error <rlang_error>
      return value must be between 5 and 10
        `.value. <= 10` is not TRUE
        
        `.value.` = dbl 11

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

# multiple assertion messages work in sanity_check()

    Code
      fun(10)
    Output
      Fatal error:
      ! x must be a positive int
        `is.integer(x)` is not TRUE
      
      i Failed an internal sanity check in package `precondition`
      i Please consider submitting a bug report
      
      
      
      ! Unable to continue, terminating
    Error <simpleError>
      terminating

---

    Code
      fun(0L)
    Output
      Fatal error:
      ! x must be a positive int
        `x > 0L` is not TRUE
      
      i Failed an internal sanity check in package `precondition`
      i Please consider submitting a bug report
      
      
      
      ! Unable to continue, terminating
    Error <simpleError>
      terminating

---

    Code
      fun(3L)
    Output
      Fatal error:
      ! x must be between 5 and 10
        `x >= 5` is not TRUE
      
      i Failed an internal sanity check in package `precondition`
      i Please consider submitting a bug report
      
      
      
      ! Unable to continue, terminating
    Error <simpleError>
      terminating

---

    Code
      fun(11L)
    Output
      Fatal error:
      ! x must be between 5 and 10
        `x <= 10` is not TRUE
      
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

