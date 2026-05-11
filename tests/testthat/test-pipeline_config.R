# Tests for R/pipeline_config.R.
# `helper-setup.R` sources `R/pipeline_config.R` and exposes `project_root()`.

test_that("check_pipeline_config() returns the expected structure", {
  withr::local_dir(project_root())
  result <- suppressWarnings(check_pipeline_config(
    required_env_vars = character(0),
    required_dirs = c("R", "scripts", "tests")
  ))
  expect_type(result, "list")
  expect_named(result, c("r_version", "project_root", "env_vars", "dirs"))
  expect_s3_class(result$r_version, "numeric_version")
  expect_true(all(result$dirs))
})

test_that("check_pipeline_config() warns when an env var is missing", {
  withr::local_dir(project_root())
  missing_var <- "SOIL_UNIT_ANALYSIS_TEST_MISSING_VAR"
  Sys.unsetenv(missing_var)
  expect_warning(
    check_pipeline_config(
      required_env_vars = missing_var,
      required_dirs = c("R")
    ),
    regexp = missing_var,
    fixed = TRUE
  )
})

test_that("check_pipeline_config() warns when a directory is missing", {
  withr::local_dir(project_root())
  bogus_dir <- "definitely-not-a-real-directory-xyzzy"
  expect_warning(
    check_pipeline_config(
      required_env_vars = character(0),
      required_dirs = bogus_dir
    ),
    regexp = bogus_dir,
    fixed = TRUE
  )
})
