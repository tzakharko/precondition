# don't show complex backtraces as it will mess with tests
options(rlang_backtrace_on_error = "none")

check_positive_int <- function(x) {
  is.integer(x) && length(x) == 1L && (x > 0) || {
    diagnose_assertion_failure(
      sprintf("`%s` must be a positive integer", forwarded_arg_label(x)),
      {{x}}
    )
  }
}

test_that("custom assertion predicate behaves as expected when called", {
  expect_silent(check_positive_int(10L))
  expect_silent(check_positive_int(-10L))

  expect_true(check_positive_int(10L))
  expect_false(check_positive_int(-10L))
})

test_that("custom assertion predicate works inside precondition()", {
  fun <- function(x) {
    precondition(check_positive_int(x))

    x
  }

  expect_silent(fun(10L))
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})