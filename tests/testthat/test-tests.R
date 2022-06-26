# don't show complex backtraces as it will mess with tests
options(rlang_backtrace_on_error = "none")

test_that("precondition() works", {
  fun <- function(x) {
    precondition(is.integer(x), x > 0L)

    x
  }

  expect_silent(fun(10L))
  expect_error(fun(10.0))
  expect_error(fun(-1L))
})


test_that("postcondition() works", {
  fun <- function(x) {
    postcondition(.value. > 0)

    x
  }

  expect_silent(fun(10))
  expect_error(fun(0))
})

test_that("sanity_check() works", {
  fun <- function(x) {
    sanity_check(x > 0)

    x
  }

  expect_silent(fun(10))
  capture.output(expect_error(fun(0)))
})

test_that("assertion message works in precondition()", {
  fun <- function(x) {
    precondition("x must be a positive int", is.integer(x), x > 0L)

    x
  }

  expect_silent(fun(10L))
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})

test_that("assertion message works in postcondition()", {  
  fun <- function(x) {
    postcondition(.value. > 0)

    x
  }

  expect_silent(fun(10))
  expect_snapshot(fun(0L), error = TRUE)
})

test_that("assertion message works in sanity_check()", {
  fun <- function(x) {
    sanity_check("x must be positive", x > 0)

    x
  }

  expect_silent(fun(10))
  expect_snapshot(fun(0L), error = TRUE)
})


test_that("embracing works in precondition()", {
  fun <- function(x) {
    precondition("x must be a positive int", is.integer({{x}}), {{x}} > 0L)

    x
  }

  expect_silent(fun(10L))
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})

test_that("embracing works in sanity_check()", {
  fun <- function(x) {
    sanity_check("x must be positive", {{x}} > 0)

    x
  }

  expect_silent(fun(10))
  expect_snapshot(fun(0L), error = TRUE)
})

