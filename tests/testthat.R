library(testthat)

test_check_dir <- file.path("..", "..")
if (file.exists(file.path(test_check_dir, "CLAUDE.md"))) {
  old_wd <- setwd(test_check_dir)
  on.exit(setwd(old_wd), add = TRUE)
}

test_dir("tests/testthat")
