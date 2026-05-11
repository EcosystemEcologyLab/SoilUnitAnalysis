#' Check pipeline configuration
#'
#' Verifies that the runtime environment satisfies the requirements declared
#' in `CLAUDE.md` and `SCIENCE_PRINCIPLES_PIPELINES.md`. Every numbered script
#' in `scripts/` must call this function before doing any work.
#'
#' Checks performed:
#' * R version meets the minimum declared in `min_r_version`.
#' * The working directory looks like the project root (contains `CLAUDE.md`
#'   and a `DESCRIPTION` file).
#' * Required project directories exist.
#' * Required environment variables are set; missing ones emit a `warning()`
#'   (not silently defaulted), per the "Fail loudly" rule in
#'   `SCIENCE_PRINCIPLES.md`.
#'
#' The function fails fast with `stop()` only on unrecoverable problems
#' (wrong R version, wrong working directory). All other issues are surfaced
#' as `warning()` so the user sees them but the pipeline can decide whether
#' to proceed.
#'
#' @param required_env_vars Character vector of environment variable names
#'   that must be set for the pipeline to run. Defaults to `"NEON_TOKEN"`
#'   per `.env.example`.
#' @param required_dirs Character vector of project-relative directories that
#'   must exist. Defaults to the directories declared in
#'   `SCIENCE_PRINCIPLES_PIPELINES.md` "Data directory conventions".
#' @param min_r_version Minimum acceptable R version as a `numeric_version`.
#'
#' @return Invisibly, a named list with elements `r_version`, `project_root`,
#'   `env_vars` (named logical: TRUE if set and non-empty), and `dirs`
#'   (named logical: TRUE if directory exists).
#' @export
check_pipeline_config <- function(
  required_env_vars = c("NEON_TOKEN"),
  required_dirs = c(
    "data/snapshots",
    "data/overrides",
    "data/raw",
    "data/extracted",
    "data/processed",
    "outputs",
    "figures"
  ),
  min_r_version = "4.4.0"
) {
  message("check_pipeline_config(): verifying pipeline configuration ...")

  current_r <- getRversion()
  if (current_r < min_r_version) {
    stop(sprintf(
      "R %s or newer is required; this session is R %s.",
      min_r_version, current_r
    ), call. = FALSE)
  }
  message(sprintf("  R version: %s (>= %s) OK", current_r, min_r_version))

  project_root <- getwd()
  if (!file.exists(file.path(project_root, "CLAUDE.md")) ||
      !file.exists(file.path(project_root, "DESCRIPTION"))) {
    stop(sprintf(
      "Working directory does not look like the project root: %s\n  Expected to find CLAUDE.md and DESCRIPTION here.",
      project_root
    ), call. = FALSE)
  }
  message(sprintf("  Project root: %s OK", project_root))

  dir_status <- vapply(
    required_dirs,
    function(d) dir.exists(file.path(project_root, d)),
    logical(1)
  )
  names(dir_status) <- required_dirs
  missing_dirs <- names(dir_status)[!dir_status]
  if (length(missing_dirs) > 0) {
    warning(sprintf(
      "Required directories missing: %s",
      paste(missing_dirs, collapse = ", ")
    ), call. = FALSE)
  } else {
    message("  Directories: all required directories present")
  }

  env_status <- vapply(
    required_env_vars,
    function(v) nzchar(Sys.getenv(v, unset = "")),
    logical(1)
  )
  names(env_status) <- required_env_vars
  missing_env <- names(env_status)[!env_status]
  if (length(missing_env) > 0) {
    warning(sprintf(
      "Required environment variables not set: %s. See .env.example.",
      paste(missing_env, collapse = ", ")
    ), call. = FALSE)
  } else {
    message("  Environment variables: all required variables set")
  }

  message("check_pipeline_config(): done.")

  invisible(list(
    r_version = current_r,
    project_root = project_root,
    env_vars = env_status,
    dirs = dir_status
  ))
}
