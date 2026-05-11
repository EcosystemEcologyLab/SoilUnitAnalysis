# Sourced by testthat before each test file.
#
# Locates the project root (the directory containing CLAUDE.md) and sources
# every R/*.R file from there. The project root is exposed as
# `project_root()` for tests that need to switch into it via
# `withr::local_dir()`.
#
# testthat resets the working directory to `tests/testthat/` before each
# `test_that()` block, so individual tests must call `withr::local_dir()` —
# setting the working directory once here would not survive.

.find_project_root <- function(start = getwd()) {
  d <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "CLAUDE.md"))) return(d)
    parent <- dirname(d)
    if (parent == d) {
      stop("Could not find project root (no CLAUDE.md in any ancestor of ", start, ").")
    }
    d <- parent
  }
}

project_root <- local({
  root <- .find_project_root()
  function() root
})

for (.f in list.files(file.path(project_root(), "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(.f)
}
