# precondition() produces expected diagnostics

    Code
      fun(10)
    Condition
      Error in `stop_assertion_failure()`:
      ! precondition failure
      * `is.integer(x)` is not TRUE
        
        is.integer(x)    logi FALSE
        x                num 10

---

    Code
      fun(0L)
    Condition
      Error in `stop_assertion_failure()`:
      ! precondition failure
      * `x > 0L` is not TRUE
        
        x > 0L    logi FALSE
        x         int 0

# postcondition() produces expected diagnostics

    Code
      fun(10)
    Condition
      Error in `stop_assertion_failure()`:
      ! postcondition failure
      * `is.integer(returnValue())` is not TRUE
        
        is.integer(returnValue())    logi FALSE
        returnValue()                num 10

---

    Code
      fun(0L)
    Condition
      Error in `stop_assertion_failure()`:
      ! postcondition failure
      * `returnValue() > 0` is not TRUE
        
        returnValue() > 0    logi FALSE
        returnValue()        int 0

# sanity_check() produces expected diagnostics

    Code
      fun(10)
    Condition
      Error in `fatal_error()`:
      ! sanity check failure
      * `is.integer(x)` is not TRUE
        
        is.integer(x)    logi FALSE
        x                num 10
      
      
      ! Failed an internal sanity check in package 'precondition'
      ! Please consider submitting a bug report
      
      x fatal error

---

    Code
      fun(0L)
    Condition
      Error in `fatal_error()`:
      ! sanity check failure
      * `x > 0L` is not TRUE
        
        x > 0L    logi FALSE
        x         int 0
      
      
      ! Failed an internal sanity check in package 'precondition'
      ! Please consider submitting a bug report
      
      x fatal error

# precondition() produces expected diagnostics on error

    Code
      fun(10L)
    Condition
      Error in `stop_assertion_failure()`:
      ! precondition failure
      * `stop("error here!")` has produced an error
        
        stop("error here!")    <error: error here!>
      
      i note: error occured when evaluating the condition

---

    Code
      fun(10)
    Condition
      Error in `stop_assertion_failure()`:
      ! precondition failure
      * `is.integer(x)` is not TRUE
        
        is.integer(x)    logi FALSE
        x                num 10

---

    Code
      fun(0L)
    Condition
      Error in `stop_assertion_failure()`:
      ! precondition failure
      * `x > 0L` is not TRUE
        
        x > 0L    logi FALSE
        x         int 0

# postcondition() produces expected diagnostics on error

    Code
      fun(10L)
    Condition
      Error in `stop_assertion_failure()`:
      ! postcondition failure
      * `stop("error here!")` has produced an error
        
        stop("error here!")    <error: error here!>
      
      i note: error occured when evaluating the condition

---

    Code
      fun(10)
    Condition
      Error in `stop_assertion_failure()`:
      ! postcondition failure
      * `is.integer(returnValue())` is not TRUE
        
        is.integer(returnValue())    logi FALSE
        returnValue()                num 10

---

    Code
      fun(0L)
    Condition
      Error in `stop_assertion_failure()`:
      ! postcondition failure
      * `returnValue() > 0` is not TRUE
        
        returnValue() > 0    logi FALSE
        returnValue()        int 0

# sanity_check() produces expected diagnostics on error

    Code
      fun(10L)
    Condition
      Error in `fatal_error()`:
      ! sanity check failure
      * `stop("error here!")` has produced an error
        
        stop("error here!")    <error: error here!>
      
      i note: error occured when evaluating the condition
      
      ! Failed an internal sanity check in package 'precondition'
      ! Please consider submitting a bug report
      
      x fatal error

---

    Code
      fun(10)
    Condition
      Error in `fatal_error()`:
      ! sanity check failure
      * `is.integer(x)` is not TRUE
        
        is.integer(x)    logi FALSE
        x                num 10
      
      
      ! Failed an internal sanity check in package 'precondition'
      ! Please consider submitting a bug report
      
      x fatal error

---

    Code
      fun(0L)
    Condition
      Error in `fatal_error()`:
      ! sanity check failure
      * `x > 0L` is not TRUE
        
        x > 0L    logi FALSE
        x         int 0
      
      
      ! Failed an internal sanity check in package 'precondition'
      ! Please consider submitting a bug report
      
      x fatal error

# precondition() produces expected diagnostics with custom messages

    Code
      fun(10)
    Condition
      Error in `stop_assertion_failure()`:
      ! precondition failure
      * `x` is int
        
        is.integer(x)    logi FALSE
        x                num 10

---

    Code
      fun(0L)
    Condition
      Error in `stop_assertion_failure()`:
      ! precondition failure
      * `x` is positive
        
        x > 0L    logi FALSE
        x         int 0

# postcondition() produces expected diagnostics with custom messages

    Code
      fun(10)
    Condition
      Error in `stop_assertion_failure()`:
      ! postcondition failure
      * returns an integer value
        
        is.integer(returnValue())    logi FALSE
        returnValue()                num 10

---

    Code
      fun(0L)
    Condition
      Error in `stop_assertion_failure()`:
      ! postcondition failure
      * returns a positive value
        
        returnValue() > 0    logi FALSE
        returnValue()        int 0

# sanity_check() produces expected diagnostics with custom messages

    Code
      fun(10)
    Condition
      Error in `fatal_error()`:
      ! sanity check failure
      * `x` is int
        
        is.integer(x)    logi FALSE
        x                num 10
      
      
      ! Failed an internal sanity check in package 'precondition'
      ! Please consider submitting a bug report
      
      x fatal error

---

    Code
      fun(0L)
    Condition
      Error in `fatal_error()`:
      ! sanity check failure
      * `x` is positive
        
        x > 0L    logi FALSE
        x         int 0
      
      
      ! Failed an internal sanity check in package 'precondition'
      ! Please consider submitting a bug report
      
      x fatal error

