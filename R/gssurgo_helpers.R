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
#' @param gdb_path Path to the `gSSURGO_AZ.gdb` directory.
#' @param aoi An `sf` or `sfc` object defining the area of interest, already
#'   in EPSG:5070. Must not be empty. Fails loudly if `aoi` is not `sf`/`sfc`
#'   or if zero polygons intersect the AOI.
#'
#' @return An `sf` polygon object of MUPOLYGON features intersecting `aoi`.
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
  result    <- sf::st_filter(mupolygon, aoi)

  if (nrow(result) == 0L) {
    stop(
      "read_gssurgo_mupolygon(): zero MUPOLYGON features intersect the supplied AOI.\n  Confirm the AOI is in EPSG:5070 before calling this function.",
      call. = FALSE
    )
  }

  result
}


#' Read the MapunitRaster_10m layer from a gSSURGO FileGeodatabase
#'
#' Reads the MapunitRaster_10m raster from the FileGeodatabase at `gdb_path`
#' using [terra::rast()], then crops and masks it to `aoi`.
#'
#' **The caller is responsible for reprojecting `aoi` to the gSSURGO native
#' CRS (EPSG:5070, CONUS Albers Equal Area) before passing it in.** This
#' function does not reproject.
#'
#' GDAL's OpenFileGDB driver (GDAL >= 3.6) is required to read rasters from
#' the FileGDB format. The function calls [terra::describe()] to enumerate
#' subdatasets and selects the one named `MapunitRaster_10m`.
#'
#' @param gdb_path Path to the `gSSURGO_AZ.gdb` directory.
#' @param aoi An `sf` or `sfc` object defining the clip extent, already in
#'   EPSG:5070. Fails loudly if `aoi` is not `sf`/`sfc` or if the clipped
#'   raster contains zero non-NA cells.
#'
#' @return A [terra::SpatRaster] of mukey values cropped and masked to `aoi`.
#' @export
read_gssurgo_raster <- function(gdb_path, aoi) {
  if (!inherits(aoi, c("sf", "sfc"))) {
    stop(sprintf(
      "aoi must be an sf or sfc object; got class '%s'.",
      paste(class(aoi), collapse = "/")
    ), call. = FALSE)
  }

  if (!dir.exists(gdb_path)) {
    stop(sprintf("GDB directory not found: %s", gdb_path), call. = FALSE)
  }

  sds <- terra::describe(gdb_path, sds = TRUE)
  rast_idx <- which(sds[["var"]] == "MapunitRaster_10m")

  if (length(rast_idx) == 0L) {
    stop(sprintf(
      "MapunitRaster_10m not found in GDB: %s\n  Available raster layers: %s",
      gdb_path,
      if (nrow(sds) == 0L) "(none)" else paste(sds[["var"]], collapse = ", ")
    ), call. = FALSE)
  }

  r       <- terra::rast(sds[["name"]][rast_idx[1L]])
  aoi_v   <- terra::vect(aoi)
  r_crop  <- terra::crop(r, aoi_v)
  r_mask  <- terra::mask(r_crop, aoi_v)

  n_valid <- terra::global(!is.na(r_mask), "sum")[[1L]]
  if (n_valid == 0L) {
    stop(
      "read_gssurgo_raster(): raster has zero non-NA cells after masking to AOI.\n  Confirm the AOI is in EPSG:5070 before calling this function.",
      call. = FALSE
    )
  }

  r_mask
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
