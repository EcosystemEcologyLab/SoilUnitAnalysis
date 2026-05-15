#' Verify a gSSURGO ZIP file by SHA-256 hash
#'
#' Confirms that the file at `path` matches the expected SHA-256 digest.
#' Fails loudly with [stop()] if the file is missing or the hash does not
#' match — a mismatched file is never silently accepted.
#'
#' @param path Path to the gSSURGO ZIP file to verify.
#' @param expected_sha256 Expected SHA-256 hex digest (64 lower-case
#'   characters). The default is the hash of `gSSURGO_AZ.zip` as published
#'   by NRCS (file timestamps in the archive: 2025-04-28 to 2025-11-20).
#'
#' @return The path, invisibly, if the hash matches.
#' @export
verify_gssurgo_zip <- function(
  path,
  # gSSURGO_AZ.zip as published by NRCS; latest archive timestamp 2025-11-20
  expected_sha256 = "d9f71fff10833851976eac2eefb1ae801df51ab2194ce2887b44f97b86bc6d04"
) {
  if (!file.exists(path)) {
    stop(sprintf(
      paste0(
        "gSSURGO ZIP not found: %s\n",
        "  Obtain gSSURGO_AZ.zip from the NRCS state databases page:\n",
        "  https://www.nrcs.usda.gov/resources/data-and-reports/",
        "gridded-soil-survey-geographic-gssurgo-database"
      ),
      path
    ), call. = FALSE)
  }

  actual_sha256 <- digest::digest(file = path, algo = "sha256")

  if (!identical(actual_sha256, expected_sha256)) {
    stop(sprintf(
      paste0(
        "SHA-256 mismatch for %s\n",
        "  expected: %s\n",
        "  actual:   %s\n",
        "  Obtain the correct gSSURGO_AZ.zip from the NRCS state databases page."
      ),
      path, expected_sha256, actual_sha256
    ), call. = FALSE)
  }

  invisible(path)
}


#' Extract the gSSURGO ZIP into a destination directory
#'
#' Extracts `zip_path` into `dest_dir` using [utils::unzip()]. Idempotent:
#' if `dest_dir/gSSURGO_AZ.gdb` already exists the extraction is skipped and
#' the path is returned immediately. After extraction the function validates
#' that the expected GDB directory is present and fails loudly if it is not.
#'
#' @param zip_path Path to the gSSURGO ZIP file.
#' @param dest_dir Directory into which the ZIP is extracted. Created if it
#'   does not exist.
#'
#' @return Path to the extracted `gSSURGO_AZ.gdb` directory, invisibly.
#' @export
extract_gssurgo_zip <- function(zip_path, dest_dir) {
  if (!file.exists(zip_path)) {
    stop(sprintf("ZIP file not found: %s", zip_path), call. = FALSE)
  }

  gdb_path <- file.path(dest_dir, "gSSURGO_AZ.gdb")

  if (dir.exists(gdb_path)) {
    message(sprintf("extract_gssurgo_zip(): GDB already exists, skipping extraction: %s", gdb_path))
    return(invisible(gdb_path))
  }

  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(zip_path, exdir = dest_dir)

  if (!dir.exists(gdb_path)) {
    stop(sprintf(
      "Extraction completed but expected GDB directory not found: %s\n  Check that the ZIP contains gSSURGO_AZ.gdb at its top level.",
      gdb_path
    ), call. = FALSE)
  }

  invisible(gdb_path)
}


#' Read MUPOLYGON feature class from a gSSURGO FileGeodatabase
#'
#' Reads the MUPOLYGON feature class from the FileGeodatabase at `gdb_path`
#' using [sf::st_read()] and spatially intersects the result with `aoi`.
#'
#' **The caller is responsible for reprojecting `aoi` to the gSSURGO native
#' CRS (EPSG:5070, CONUS Albers Equal Area) before passing it in.** This
#' function does not reproject, keeping the CRS used for clipping explicit in
#' the calling script.
#'
#' The MUPOLYGON feature class uses an uppercase `MUKEY` column; this function
#' normalizes it to lowercase `mukey` so callers can assume lowercase throughout
#' (matching the tabular tables `Valu1`, `component`, and `chorizon`).
#'
#' @param gdb_path Path to the `gSSURGO_AZ.gdb` directory.
#' @param aoi An `sf` or `sfc` object defining the area of interest, already
#'   in EPSG:5070. Must not be empty. Fails loudly if `aoi` is not `sf`/`sfc`
#'   or if zero polygons intersect the AOI.
#'
#' @return An `sf` polygon object of MUPOLYGON features intersecting `aoi`,
#'   with the key column normalized to lowercase `mukey`.
#' @export
read_gssurgo_mupolygon <- function(gdb_path, aoi) {
  if (!inherits(aoi, c("sf", "sfc"))) {
    stop(sprintf(
      "aoi must be an sf or sfc object; got class '%s'.",
      paste(class(aoi), collapse = "/")
    ), call. = FALSE)
  }

  if (!dir.exists(gdb_path)) {
    stop(sprintf("GDB directory not found: %s", gdb_path), call. = FALSE)
  }

  mupolygon <- sf::st_read(gdb_path, layer = "MUPOLYGON", quiet = TRUE)

  # MUPOLYGON ships with uppercase MUKEY; normalize to lowercase so all
  # downstream code can assume mukey (matching Valu1, component, chorizon).
  names(mupolygon)[names(mupolygon) == "MUKEY"] <- "mukey"
  if (!"mukey" %in% names(mupolygon)) {
    stop(sprintf(
      "read_gssurgo_mupolygon(): 'mukey' column not found after MUKEY->mukey normalization.\n  Actual column names: %s",
      paste(names(mupolygon), collapse = ", ")
    ), call. = FALSE)
  }

  result <- sf::st_filter(mupolygon, aoi)

  if (nrow(result) == 0L) {
    stop(
      "read_gssurgo_mupolygon(): zero MUPOLYGON features intersect the supplied AOI.\n  Confirm the AOI is in EPSG:5070 before calling this function.",
      call. = FALSE
    )
  }

  result
}


#' Rasterize MUPOLYGON to an integer mukey grid
#'
#' Generates a rasterized representation of MUPOLYGON at the requested
#' resolution by calling [terra::rasterize()]. **This is not the official NRCS
#' MapunitRaster_10m.** The GDAL 3.8.4 OpenFileGDB driver in this environment
#' does not expose FileGDB raster layers, so the raster is generated from the
#' MUPOLYGON vector instead.
#'
#' The output is functionally equivalent to MapunitRaster_10m for the
#' rasterized area — cell values are the same mukeys placed on the same 10 m
#' grid — but is not bitwise-identical: sub-cell boundary placement may differ.
#'
#' The template raster extent is snapped so the bounding box of `mupolygon` is
#' an integer multiple of `resolution` in EPSG:5070. This ensures downstream
#' alignment with AOP rasters defined on the same EPSG:5070 grid.
#'
#' @param mupolygon An `sf` polygon object with a `"mukey"` column, in
#'   EPSG:5070 (the gSSURGO native CRS). Typically the output of
#'   [read_gssurgo_mupolygon()].
#' @param resolution Cell size in metres. Default is `10`.
#'
#' @return A [terra::SpatRaster] of integer mukey values covering `mupolygon`,
#'   at `resolution` metres, in EPSG:5070.
#' @export
rasterize_mupolygon <- function(mupolygon, resolution = 10) {
  if (!inherits(mupolygon, "sf")) {
    stop(sprintf(
      "mupolygon must be an sf object; got class '%s'.",
      paste(class(mupolygon), collapse = "/")
    ), call. = FALSE)
  }

  if (!"mukey" %in% names(mupolygon)) {
    stop(sprintf(
      "rasterize_mupolygon(): mupolygon must have a 'mukey' column.\n  Actual column names: %s",
      paste(names(mupolygon), collapse = ", ")
    ), call. = FALSE)
  }

  bb   <- sf::st_bbox(mupolygon)
  xmin <- floor(bb["xmin"]   / resolution) * resolution
  ymin <- floor(bb["ymin"]   / resolution) * resolution
  xmax <- ceiling(bb["xmax"] / resolution) * resolution
  ymax <- ceiling(bb["ymax"] / resolution) * resolution

  template <- terra::rast(
    xmin       = xmin, xmax = xmax,
    ymin       = ymin, ymax = ymax,
    resolution = resolution,
    crs        = terra::crs(terra::vect(sf::st_geometry(mupolygon)))
  )

  v        <- terra::vect(mupolygon)
  v$mukey  <- as.integer(as.character(v$mukey))
  r        <- terra::rasterize(v, template, field = "mukey")

  n_valid <- terra::global(!is.na(r), "sum")[[1L]]
  if (n_valid == 0L) {
    stop(
      "rasterize_mupolygon(): rasterized output has zero non-NA cells.\n  Confirm mupolygon is in EPSG:5070 and is not empty.",
      call. = FALSE
    )
  }

  r
}


#' Read a tabular layer from a gSSURGO FileGeodatabase
#'
#' Reads a non-spatial tabular layer from the FileGeodatabase at `gdb_path`
#' using [sf::st_read()] with `layer = table_name` (sf reads non-spatial
#' tables from a GDB this way). Filters rows where the column named `key_col`
#' is in `key_values`. Returns a plain data frame.
#'
#' **Join responsibility:** This function handles direct key filtering only.
#' The gSSURGO schema relationships are:
#' - `Valu1.mukey` links directly to `mapunit.mukey` — use `key_col = "mukey"`.
#' - `component.mukey` links to `mapunit.mukey` — use `key_col = "mukey"`.
#' - `chorizon.cokey` links to `component.cokey`; there is no `mukey` column
#'   in `chorizon` — use `key_col = "cokey"` and supply component cokeys.
#'
#' The two-step join (`mukey → cokey → chkey`) must be performed explicitly
#' in the calling script so the join logic is visible in the pipeline.
#'
#' @param gdb_path Path to the `gSSURGO_AZ.gdb` directory.
#' @param table_name Name of the GDB table or feature class to read (e.g.
#'   `"Valu1"`, `"component"`, `"chorizon"`).
#' @param key_values Character or integer vector of key values to keep. Rows
#'   where `key_col` is not in `key_values` are dropped.
#' @param key_col Name of the column in `table_name` used for filtering.
#'   Defaults to `"mukey"` (correct for `Valu1` and `component`). Use
#'   `"cokey"` when reading `chorizon`.
#'
#' @return A data frame of rows matching `key_values`.
#' @export
read_gssurgo_table <- function(gdb_path, table_name, key_values,
                               key_col = "mukey") {
  if (!dir.exists(gdb_path)) {
    stop(sprintf("GDB directory not found: %s", gdb_path), call. = FALSE)
  }

  available <- tryCatch(
    sf::st_layers(gdb_path)$name,
    error = function(e) {
      stop(sprintf(
        "Could not list layers in GDB: %s\n  %s",
        gdb_path, conditionMessage(e)
      ), call. = FALSE)
    }
  )

  if (!table_name %in% available) {
    stop(sprintf(
      "Table '%s' not found in GDB: %s\n  Available layers: %s",
      table_name, gdb_path, paste(available, collapse = ", ")
    ), call. = FALSE)
  }

  tbl <- sf::st_read(gdb_path, layer = table_name, quiet = TRUE)
  tbl <- as.data.frame(tbl)

  if (!key_col %in% names(tbl)) {
    stop(sprintf(
      "Column '%s' not found in table '%s'.\n  Available columns: %s",
      key_col, table_name, paste(names(tbl), collapse = ", ")
    ), call. = FALSE)
  }

  tbl[as.character(tbl[[key_col]]) %in% as.character(key_values), ]
}
