# Tests for R/soilweb_helpers.R.
# `helper-setup.R` sources all R/*.R files and exposes `project_root()`.

# --- load_srer_boundary() ----------------------------------------------------

test_that("load_srer_boundary() stops when the shapefile is missing", {
  missing_path <- tempfile(fileext = ".shp")
  expect_error(
    load_srer_boundary(path = missing_path),
    regexp = "not found",
    fixed  = FALSE
  )
})

# --- query_ssurgo_polygons() --------------------------------------------------

test_that("query_ssurgo_polygons() stops on non-sf non-sfc input", {
  expect_error(
    query_ssurgo_polygons(data.frame(x = 1, y = 2)),
    regexp = "sf or sfc object"
  )
  expect_error(
    query_ssurgo_polygons("not an sf"),
    regexp = "sf or sfc object"
  )
  expect_error(
    query_ssurgo_polygons(list(x = 1)),
    regexp = "sf or sfc object"
  )
})

test_that("query_ssurgo_polygons() accepts sfc input (passes type check)", {
  aoi_sfc <- sf::st_as_sfc(
    sf::st_bbox(
      c(xmin = -110.855, ymin = 31.825, xmax = -110.845, ymax = 31.835),
      crs = 4326
    )
  )

  err <- tryCatch(
    { query_ssurgo_polygons(aoi_sfc); NULL },
    error = function(e) e
  )

  # If the call errored, it must NOT be the type-check error — the sfc passed
  # validation and the failure came from somewhere downstream (e.g. SDA network).
  if (!is.null(err)) {
    expect_false(
      grepl("aoi must be an sf or sfc object", conditionMessage(err), fixed = TRUE)
    )
  }
})

# --- query_ssurgo_properties() ------------------------------------------------

test_that("query_ssurgo_properties() stops on empty mukeys", {
  expect_error(
    query_ssurgo_properties(mukeys = character(0), properties = "claytotal_r"),
    regexp = "empty"
  )
  expect_error(
    query_ssurgo_properties(mukeys = integer(0), properties = "claytotal_r"),
    regexp = "empty"
  )
})

# --- write_provenance_sidecar() -----------------------------------------------

test_that("write_provenance_sidecar() writes a parseable YAML with required fields", {
  tmp_output   <- tempfile(fileext = ".csv")
  sources      <- list(boundary = "data/snapshots/srer_boundary/srer_boundary.shp")
  notes_text   <- "unit test run — no real data"

  sidecar_path <- write_provenance_sidecar(
    output_path   = tmp_output,
    input_sources = sources,
    notes         = notes_text
  )

  expect_equal(sidecar_path, paste0(tmp_output, ".provenance.yaml"))
  expect_true(file.exists(sidecar_path))

  parsed <- yaml::read_yaml(sidecar_path)

  expect_equal(parsed$notes, notes_text)
  expect_equal(parsed$input_sources, sources)
  expect_true("output_file"      %in% names(parsed))
  expect_true("run_datetime_utc" %in% names(parsed))
  expect_true("pipeline_version" %in% names(parsed))
  expect_true("r_session_info"   %in% names(parsed))
  expect_equal(parsed$output_file, basename(tmp_output))

  on.exit(unlink(sidecar_path))
})

test_that("write_provenance_sidecar() returns the sidecar path invisibly", {
  tmp_output <- tempfile(fileext = ".csv")
  result     <- withVisible(
    write_provenance_sidecar(tmp_output, input_sources = list(), notes = "")
  )
  expect_false(result$visible)
  expect_equal(result$value, paste0(tmp_output, ".provenance.yaml"))
  on.exit(unlink(result$value))
})

# --- end-to-end network test --------------------------------------------------

test_that("query_ssurgo_polygons() returns sf polygons for a small SRER AOI [network]", {
  skip_on_cran()
  skip_if_offline(host = "sdmdataaccess.nrcs.usda.gov")

  # ~1 km² box centred on SRER headquarters (~31.83 N, -110.85 W)
  aoi <- sf::st_sf(
    geometry = sf::st_as_sfc(
      sf::st_bbox(
        c(xmin = -110.855, ymin = 31.825, xmax = -110.845, ymax = 31.835),
        crs = 4326
      )
    )
  )

  result <- tryCatch(
    query_ssurgo_polygons(aoi),
    error = function(e) {
      skip(paste("SDA query failed (possibly transient):", conditionMessage(e)))
    }
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0L)
  expect_false(is.null(attr(result, "query_datetime_utc")))
  expect_false(is.null(attr(result, "aoi_wkt")))
})
