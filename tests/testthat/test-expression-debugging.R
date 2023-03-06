# don't show complex backtraces as it will mess with tests
options(rlang_backtrace_on_error = "none")

test_that("diagnose_expressions() correctly captures basic expressions", {
  x <- 1
  y <- 2

  expected_result <- unsafe_data_frame(
    expr = list(quote(x + 1), quote(y + 1)),
    eval_result = list(x+1, y+1),
    is_error = c(FALSE, FALSE)
  )

  expect_identical(diagnose_expressions(x + 1, y + 1), expected_result)
})

test_that("diagnose_expressions() correctly captures errors", {
  x <- 1
  y <- 2

  expected_result <- unsafe_data_frame(
    expr = list(quote(x + 1), quote(stop("error!")), quote(y + 1)),
    eval_result = list(x+1, simpleError("error!", call=quote(stop("error!"))), y+1),
    is_error = c(FALSE, TRUE, FALSE)
  )

  expect_identical(diagnose_expressions(x + 1, stop("error!"), y + 1), expected_result)
})


test_that("diagnose_expressions() correctly substitutes and extracts embraced expressions", {
  x <- 1
  y <- 2

  expected_result <- unsafe_data_frame(
    expr = list(quote(x + 1), quote(y + 1), quote(x), quote(y)),
    eval_result = list(x+1, y+1, x, y),
    is_error = c(FALSE, FALSE, FALSE, FALSE)
  )

  # expect_identical does argument processing, so we have to 
  # escape the call to diagnose_expressions()
  expect_identical(!!quote(diagnose_expressions({x} + 1, {y} + 1)), expected_result)
})

test_that("diagnose_expressions() correctly substitutes forwarded arguments", {
  parent <- function(x) {
    leaf(x)
  }

  leaf <- function(y) {
    diagnose_expressions({{{y}}} + 1)
  }

  val <- 1

  expected_result <- unsafe_data_frame(
    expr = list(quote(x + 1), quote(x)),
    eval_result = list(val+1, val),
    is_error = c(FALSE, FALSE)
  )

  expect_identical(parent(val), expected_result)
})


test_that("diagnose_expressions() correctly substitutes forwarded arguments accross multiple calls", {
  parent1 <- function(x) {
    parent2(x)
  }

  parent2 <- function(x1) {
    parent3({{x1}})
  }

  parent3 <- function(x2) {
    leaf({{x2}})
  }

  leaf <- function(y) {
    diagnose_expressions({{{y}}} + 1)
  }

  val <- 1

  expected_result <- unsafe_data_frame(
    expr = list(quote(x + 1), quote(x)),
    eval_result = list(val+1, val),
    is_error = c(FALSE, FALSE)
  )

  expect_identical(parent1(val), expected_result)
})


test_that("diagnose_expressions() correctly substitutes arguments in a different environment", {
  parent1 <- function(x) {
    parent2(x)
  }

  parent2 <- function(x1) {
    parent3({{x1}})
  }

  parent3 <- function(x2) {
    leaf({{x2}})
  }

  leaf <- function(y) {
    invoke(environment())
  }

  invoke <- function(env) {
    evalq(diagnose_expressions({{{y}}} + 1), env)
  }

  val <- 1

  expected_result <- unsafe_data_frame(
    expr = list(quote(x + 1), quote(x)),
    eval_result = list(val+1, val),
    is_error = c(FALSE, FALSE)
  )

  expect_identical(parent1(val), expected_result)
})


