# fatal_error() produces expected output with fatal_error_action = NULL

    Code
      fatal_error(c("error", "with", "bullets", "", i = "and notes"))
    Condition
      Error in `fatal_error()`:
      ! error
      with
      bullets
      
      i and notes
      
      x fatal error
    Output
      <error/precondition/fatal_error>
      Error in `fatal_error()`:
      ! error
      with
      bullets
      
      i and notes
      
      ! an error raised by `fatal_error()` was handled in `tryCatch()`
      i fatal errors indicate critical failures in the program
      i and should typically abort the program execution
      
      i use `option(fatal_error_action = "none")` to silence this message
      i see ?precondition_fatal_error_action

# fatal_error() produces a note inside tryCatch() when fatal_error_action = NULL

    Code
      tryCatch(fatal_error(c("error", "with", "bullets", "", i = "and notes")),
      error = function(...) NULL)
    Output
      <error/precondition/fatal_error>
      Error in `fatal_error()`:
      ! error
      with
      bullets
      
      i and notes
      
      ! an error raised by `fatal_error()` was handled in `tryCatch()`
      i fatal errors indicate critical failures in the program
      i and should typically abort the program execution
      
      i use `option(fatal_error_action = "none")` to silence this message
      i see ?precondition_fatal_error_action
      NULL

# fatal_error() produces expected output with fatal_error_action = 'info'

    Code
      fatal_error(c("error", "with", "bullets", "", i = "and notes"))
    Condition
      Error in `fatal_error()`:
      ! error
      with
      bullets
      
      i and notes
      
      x fatal error
    Output
      <error/precondition/fatal_error>
      Error in `fatal_error()`:
      ! error
      with
      bullets
      
      i and notes
      
      ! an error raised by `fatal_error()` was handled in `tryCatch()`
      i fatal errors indicate critical failures in the program
      i and should typically abort the program execution
      
      i use `option(fatal_error_action = "none")` to silence this message
      i see ?precondition_fatal_error_action

# fatal_error() produces a note inside tryCatch() when fatal_error_action = 'info'

    Code
      tryCatch(fatal_error(c("error", "with", "bullets", "", i = "and notes")),
      error = function(...) NULL)
    Output
      <error/precondition/fatal_error>
      Error in `fatal_error()`:
      ! error
      with
      bullets
      
      i and notes
      
      ! an error raised by `fatal_error()` was handled in `tryCatch()`
      i fatal errors indicate critical failures in the program
      i and should typically abort the program execution
      
      i use `option(fatal_error_action = "none")` to silence this message
      i see ?precondition_fatal_error_action
      NULL

# fatal_error() produces expected output with fatal_error_action = 'none'

    Code
      fatal_error(c("error", "with", "bullets", "", i = "and notes"))
    Condition
      Error in `fatal_error()`:
      ! error
      with
      bullets
      
      i and notes
      
      x fatal error

# fatal_error() calls quit() when when fatal_error_action = 'terminate'

    Code
      fatal_error(c("error", "with", "bullets", "", i = "and notes"))
    Output
      <error/precondition/fatal_error>
      Error in `fatal_error()`:
      ! error
      with
      bullets
      
      i and notes
      
      x fatal error, execution halted
    Condition
      Error in `quit()`:
      ! quit() called

# fatal_error() calls quit() inside tryCatch() when when fatal_error_action = 'terminate'

    Code
      tryCatch(fatal_error(c("error", "with", "bullets", "", i = "and notes")),
      error = function(cnd) {
        if (inherits(cnd, "modular/quit_action")) rlang::cnd_signal(cnd)
        NULL
      })
    Output
      <error/precondition/fatal_error>
      Error in `fatal_error()`:
      ! error
      with
      bullets
      
      i and notes
      
      x fatal error, execution halted
    Condition
      Error in `quit()`:
      ! quit() called

