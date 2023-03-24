# don't show complex backtraces as it will mess with tests
options(rlang_backtrace_on_error = "none")


test_that("fatal_error() produces a fatal error", {
  withr::local_options(fatal_error_action = NULL)
  withr::local_output_sink(textConnection(NULL, open = "w"))

  expect_error(fatal_error("error"), class="precondition/fatal_error")
})

test_that("fatal_error() produces expected output with fatal_error_action = NULL", {
  withr::local_options(fatal_error_action = NULL)

  expect_snapshot(fatal_error(c("error", "with", "bullets", "", "i" = "and notes")), error = TRUE)
})

test_that("fatal_error() produces a note inside tryCatch() when fatal_error_action = NULL", {
  withr::local_options(fatal_error_action = NULL)

  expect_snapshot(tryCatch(
    fatal_error(c("error", "with", "bullets", "", "i" = "and notes")),
    error = function(...) NULL
  ))
})


test_that("fatal_error() produces expected output with fatal_error_action = 'info'", {
  withr::local_options(fatal_error_action = 'inform')

  expect_snapshot(fatal_error(c("error", "with", "bullets", "", "i" = "and notes")), error = TRUE)
})

test_that("fatal_error() produces a note inside tryCatch() when fatal_error_action = 'info'", {
  withr::local_options(fatal_error_action = 'inform')

  expect_snapshot(tryCatch(
    fatal_error(c("error", "with", "bullets", "", "i" = "and notes")),
    error = function(...) NULL
  ))
})


test_that("fatal_error() produces expected output with fatal_error_action = 'none'", {
  withr::local_options(fatal_error_action = 'none')

  expect_snapshot(fatal_error(c("error", "with", "bullets", "", "i" = "and notes")), error = TRUE)
})

test_that("fatal_error() produces no output inside tryCatch() when fatal_error_action = 'none'", {
  withr::local_options(fatal_error_action = 'none')

  expect_silent(tryCatch(
    fatal_error(c("error", "with", "bullets", "", "i" = "and notes")),
    error = function(...) NULL
  ))
})


test_that("fatal_error() calls quit() when when fatal_error_action = 'terminate'", {
  skip_on_cran()

  withr::local_options(fatal_error_action = 'terminate')
  
  local_mocked_bindings(quit = function(...) {
    rlang::abort("quit() called", class="modular/quit_action", trace = NULL)
  }, .package = "base")

  expect_snapshot(fatal_error(c("error", "with", "bullets", "", "i" = "and notes")), error = TRUE)
})


test_that("fatal_error() calls quit() inside tryCatch() when when fatal_error_action = 'terminate'", {
  skip_on_cran()
  withr::local_options(fatal_error_action = 'terminate')

  local_mocked_bindings(quit = function(...) {
    rlang::abort("quit() called", class="modular/quit_action", trace = NULL)
  }, .package = "base")

  expect_snapshot(tryCatch(
    fatal_error(c("error", "with", "bullets", "", "i" = "and notes")),
    "error" = function(cnd) {
      if(inherits(cnd, "modular/quit_action")) rlang::cnd_signal(cnd)
      NULL
    }
  ), error = TRUE)
})

