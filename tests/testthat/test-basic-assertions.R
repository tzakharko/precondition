# don't show complex backtraces as it will mess with tests
options(rlang_backtrace_on_error = "none")
options(fatal_error_action = "none")

test_that("precondition() works as expected", {
  fun <- function(x) {
    precondition(is.integer({x}), {x} > 0L)

    x
  }

  expect_silent(fun(10L))
  expect_error(fun(10.0), class="precondition/assertion_error")
  expect_error(fun(-1L), class="precondition/assertion_error")
})


test_that("postcondition() works as expected", {
  fun <- function(x) {
    postcondition(is.integer({returnValue()}), {returnValue()} > 0)

    x
  }

  expect_silent(fun(10L))
  expect_error(fun(10.0), class="precondition/assertion_error")
  expect_error(fun(0L), class="precondition/assertion_error")
})

test_that("sanity_check() works as expected", {
  fun <- function(x) {
    sanity_check(is.integer({x}), {x} > 0L)

    x
  }

  

  expect_silent(fun(10L))
  capture.output(expect_error(fun(10.0)))
  capture.output(expect_error(fun(-1L)))
})

test_that("precondition() produces expected diagnostics", {
  fun <- function(x) {
    precondition(is.integer({x}), {x} > 0L)

    x
  }

  expect_silent(fun(10L))
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})


test_that("postcondition() produces expected diagnostics", {
  fun <- function(x) {
    postcondition(is.integer({returnValue()}), {returnValue()} > 0)

    x
  }

  expect_silent(fun(10L))
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})

test_that("sanity_check() produces expected diagnostics", {
  fun <- function(x) {
    sanity_check(is.integer({x}), {x} > 0L)

    x
  }

  expect_silent(fun(10L))
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})


test_that("precondition() produces expected diagnostics on error", {
  fun <- function(x) {
    precondition(is.integer({x}), {x} > 0L, stop("error here!"))

    x
  }

  expect_snapshot(fun(10L), error = TRUE)
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})


test_that("postcondition() produces expected diagnostics on error", {
  fun <- function(x) {
    postcondition(is.integer({returnValue()}), {returnValue()} > 0, stop("error here!"))

    x
  }

  expect_snapshot(fun(10L), error = TRUE)
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})

test_that("sanity_check() produces expected diagnostics on error", {
  fun <- function(x) {
    sanity_check(is.integer({x}), {x} > 0L, stop("error here!"))

    x
  }

  expect_snapshot(fun(10L), error = TRUE)
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})



test_that("precondition() produces expected diagnostics with custom messages", {
  fun <- function(x) {
    precondition("`x` is int", is.integer({x}), "`x` is positive", {x} > 0L)

    x
  }

  expect_silent(fun(10L))
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})


test_that("postcondition() produces expected diagnostics with custom messages", {
  fun <- function(x) {
    postcondition("returns an integer value", is.integer({returnValue()}),
                  "returns a positive value", {returnValue()} > 0)

    x
  }

  expect_silent(fun(10L))
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})

test_that("sanity_check() produces expected diagnostics with custom messages", {
  fun <- function(x) {
    sanity_check("`x` is int", is.integer({x}), "`x` is positive", {x} > 0L)

    x
  }
  
  expect_silent(fun(10L))
  expect_snapshot(fun(10.0), error = TRUE)
  expect_snapshot(fun(0L), error = TRUE)
})