#' Load SRER station boundary shapefile
#'
#' Reads the Santa Rita Experimental Range boundary from a local shapefile,
#' validates that the file exists, that the CRS is defined, and that the
#' result contains exactly one feature. If the file contains multiple
#' features they are dissolved to a single polygon with [sf::st_union()] and
#' a warning is emitted. Fails loudly on a missing file or an undefined CRS.
#'
#' CRS is returned as-is. Reprojection is intentionally left to the caller
#' so the CRS used downstream is explicit in the calling script rather than
#' hidden inside this helper.
#'
#' @param path Path to the `.shp` file. The default is relative to the
#'   project root; scripts must set the working directory to the project root
#'   before calling this function (see `R/pipeline_config.R`).
#'
#' @return An `sf` object with one polygon feature and the CRS as stored in
#'   the accompanying `.prj` sidecar.
#' @export
load_srer_boundary <- function(
  path = "data/snapshots/srer_boundary/srer_boundary.shp"
) {
  if (!file.exists(path)) {
    stop(sprintf("Boundary shapefile not found: %s", path), call. = FALSE)
  }

  boundary <- sf::st_read(path, quiet = TRUE)

  if (is.na(sf::st_crs(boundary))) {
    stop(sprintf(
      "Boundary shapefile has no defined CRS: %s\n  Check the .prj sidecar.",
      path
    ), call. = FALSE)
  }

  if (nrow(boundary) > 1L) {
    warning(sprintf(
      "Boundary shapefile contains %d features; dissolving to a single polygon with st_union().",
      nrow(boundary)
    ), call. = FALSE)
    boundary <- sf::st_sf(geometry = sf::st_union(boundary))
  }

  boundary
}


#' Write a provenance sidecar YAML file
#'
#' Records pipeline provenance metadata alongside an output file, per the
#' requirements in `SCIENCE_PRINCIPLES_PIPELINES.md`. The sidecar is written
#' to `paste0(output_path, ".provenance.yaml")`.
#'
#' Required metadata fields written:
#' - `output_file`: basename of `output_path`.
#' - `run_datetime_utc`: ISO 8601 UTC timestamp.
#' - `pipeline_version`: short git commit hash, or `"unknown"` if git is
#'   unavailable or the working directory is not a git repository.
#' - `input_sources`: the supplied list, passed through unchanged.
#' - `r_session_info`: output of [utils::sessionInfo()].
#' - `notes`: the supplied string (required even if empty).
#'
#' @param output_path Path to the primary output file. The sidecar is written
#'   to `paste0(output_path, ".provenance.yaml")`.
#' @param input_sources A named list of input source descriptors (URLs, DOIs,
#'   or file paths). Passed through to the YAML unchanged.
#' @param notes Free-text string documenting manual decisions, overrides, or
#'   deviations from defaults. An empty string is acceptable; the field must
#'   be present.
#'
#' @return The sidecar path, invisibly.
#' @export
write_provenance_sidecar <- function(output_path, input_sources, notes) {
  pipeline_version <- tryCatch({
    v <- suppressWarnings(
      system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE)
    )
    if (length(v) == 0L || !nzchar(v[1L])) "unknown" else v[1L]
  }, error = function(e) "unknown")

  sidecar <- list(
    output_file      = basename(output_path),
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = pipeline_version,
    input_sources    = input_sources,
    r_session_info   = utils::capture.output(utils::sessionInfo()),
    notes            = notes
  )

  sidecar_path <- paste0(output_path, ".provenance.yaml")
  yaml::write_yaml(sidecar, sidecar_path)

  invisible(sidecar_path)
}
