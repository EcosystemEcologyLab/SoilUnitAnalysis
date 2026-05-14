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


#' Query SSURGO map unit polygons from SDA
#'
#' Wraps [soilDB::SDA_spatialQuery()] to retrieve SSURGO map unit polygons
#' that intersect a supplied area of interest. Polygons are clipped
#' server-side (`geomIntersection = TRUE`) to keep the download payload
#' small. The AOI is reprojected to EPSG:4326 (WGS 84) before the query
#' because SDA expects geographic coordinates.
#'
#' Two provenance attributes are attached to the returned object:
#' - `query_datetime_utc`: ISO 8601 UTC timestamp of the query.
#' - `aoi_wkt`: WKT representation of the (unioned, WGS 84) AOI geometry.
#'
#' @param aoi An `sf` or `sfc` object defining the area of interest. Both
#'   types are accepted; `sfc` is the natural output of [sf::st_as_sfc()] on
#'   a bounding box. The object must have a defined CRS; any projection is
#'   accepted and will be transformed internally. Fails loudly if `aoi` is
#'   neither `sf` nor `sfc`, if the service is unreachable (the underlying
#'   SDA error message is forwarded), or if the query returns zero features.
#' @param db SDA database to query. Passed to [soilDB::SDA_spatialQuery()].
#'   Defaults to `"SSURGO"`.
#'
#' @return An `sf` polygon object of SSURGO map unit boundaries intersecting
#'   the AOI, with `query_datetime_utc` and `aoi_wkt` attached as attributes.
#' @export
query_ssurgo_polygons <- function(aoi, db = "SSURGO") {
  if (!inherits(aoi, c("sf", "sfc"))) {
    stop(
      sprintf(
        "aoi must be an sf or sfc object; got class '%s'.",
        paste(class(aoi), collapse = "/")
      ),
      call. = FALSE
    )
  }

  aoi_4326 <- sf::st_transform(aoi, crs = 4326)

  query_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  aoi_wkt    <- sf::st_as_text(sf::st_union(aoi_4326))

  result <- soilDB::SDA_spatialQuery(
    geom             = aoi_4326,
    what             = "mupolygon",
    geomIntersection = TRUE,
    db               = db
  )

  if (inherits(result, "try-error")) {
    cond <- attr(result, "condition")
    msg  <- if (!is.null(cond)) conditionMessage(cond) else as.character(result)
    stop("soilDB::SDA_spatialQuery() failed: ", msg, call. = FALSE)
  }

  if (is.null(result) || nrow(result) == 0L) {
    stop(
      "SDA_spatialQuery() returned zero features for the supplied AOI.",
      call. = FALSE
    )
  }

  attr(result, "query_datetime_utc") <- query_time
  attr(result, "aoi_wkt")            <- aoi_wkt

  result
}


#' Query SSURGO tabular properties from SDA
#'
#' Wraps [soilDB::get_SDA_property()] to retrieve one or more soil properties
#' aggregated over a specified depth interval for a vector of map unit keys
#' (mukeys). The mukey vector is split into chunks of at most 5 000 keys per
#' request to stay within SDA's request-size ceiling (soilDB issue #228
#' documents failures above approximately 15 000 mukeys per call). Results
#' from all chunks are row-bound into a single data frame.
#'
#' **Provisional defaults — confirm with PI before using in analysis:**
#' The `method`, `top_depth`, and `bottom_depth` defaults (`"Weighted
#' Average"`, `0`–`25` cm) are starting points only. The depth interval and
#' aggregation method are scientific choices with direct consequences for
#' which soil horizon is represented; they must be confirmed by the PI before
#' results are used in any downstream analysis.
#'
#' @param mukeys Integer or character vector of SSURGO map unit keys. Must
#'   not be empty.
#' @param properties Character vector of SDA property names to retrieve
#'   (e.g. `"claytotal_r"`, `"sandtotal_r"`). Passed to the `property`
#'   argument of [soilDB::get_SDA_property()].
#' @param method Aggregation method string accepted by
#'   [soilDB::get_SDA_property()]. **PROVISIONAL default:** `"Weighted
#'   Average"` — confirm with PI.
#' @param top_depth Top of the depth interval in cm. **PROVISIONAL default:**
#'   `0` — confirm with PI.
#' @param bottom_depth Bottom of the depth interval in cm. **PROVISIONAL
#'   default:** `25` — confirm with PI.
#'
#' @return A data frame of soil properties, one row per mukey (or per
#'   mukey × component depending on `method`), row-bound across all chunks.
#' @export
query_ssurgo_properties <- function(
  mukeys,
  properties,
  method       = "Weighted Average",
  top_depth    = 0,
  bottom_depth = 25
) {
  if (length(mukeys) == 0L) {
    stop("mukeys must not be empty.", call. = FALSE)
  }

  chunks <- soilDB::makeChunks(mukeys, size = 5000L)

  results <- lapply(unique(chunks), function(chunk_id) {
    soilDB::get_SDA_property(
      property     = properties,
      method       = method,
      mukeys       = mukeys[chunks == chunk_id],
      top_depth    = top_depth,
      bottom_depth = bottom_depth
    )
  })

  do.call(rbind, Filter(Negate(is.null), results))
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
