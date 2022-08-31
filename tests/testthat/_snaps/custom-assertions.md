# custom assertion predicate works inside precondition()

    Code
      fun(10)
    Condition
      Error in `stop_assertion_failure()`:
      ! precondition failure
      * `x` must be a positive integer
        
        x    num 10

---

    Code
      fun(0L)
    Condition
      Error in `stop_assertion_failure()`:
      ! precondition failure
      * `x` must be a positive integer
        
        x    int 0

