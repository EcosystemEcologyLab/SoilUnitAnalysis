# Tests for R/ssurgo_helpers.R and R/gssurgo_helpers.R.
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

# --- verify_gssurgo_zip() ----------------------------------------------------

test_that("verify_gssurgo_zip() stops when the file is missing", {
  missing_path <- tempfile(fileext = ".zip")
  expect_error(
    verify_gssurgo_zip(path = missing_path),
    regexp = "not found"
  )
})

test_that("verify_gssurgo_zip() stops when the hash does not match", {
  tmp_zip <- tempfile(fileext = ".zip")
  writeLines("not a real zip", tmp_zip)
  on.exit(unlink(tmp_zip))

  expect_error(
    verify_gssurgo_zip(path = tmp_zip, expected_sha256 = "0000000000000000"),
    regexp = "SHA-256 mismatch"
  )
})

# --- extract_gssurgo_zip() ---------------------------------------------------

test_that("extract_gssurgo_zip() stops when the ZIP file is missing", {
  missing_zip <- tempfile(fileext = ".zip")
  dest        <- tempdir()
  expect_error(
    extract_gssurgo_zip(zip_path = missing_zip, dest_dir = dest),
    regexp = "not found"
  )
})

# --- read_gssurgo_mupolygon() ------------------------------------------------

test_that("read_gssurgo_mupolygon() stops on non-sf non-sfc AOI", {
  fake_gdb <- tempfile()
  dir.create(fake_gdb)
  on.exit(unlink(fake_gdb, recursive = TRUE))

  expect_error(
    read_gssurgo_mupolygon(fake_gdb, aoi = data.frame(x = 1)),
    regexp = "sf or sfc object"
  )
  expect_error(
    read_gssurgo_mupolygon(fake_gdb, aoi = "not sf"),
    regexp = "sf or sfc object"
  )
  expect_error(
    read_gssurgo_mupolygon(fake_gdb, aoi = list(x = 1)),
    regexp = "sf or sfc object"
  )
})

# --- read_gssurgo_raster() ---------------------------------------------------

test_that("read_gssurgo_raster() stops on non-sf non-sfc AOI", {
  fake_gdb <- tempfile()
  dir.create(fake_gdb)
  on.exit(unlink(fake_gdb, recursive = TRUE))

  expect_error(
    read_gssurgo_raster(fake_gdb, aoi = data.frame(x = 1)),
    regexp = "sf or sfc object"
  )
  expect_error(
    read_gssurgo_raster(fake_gdb, aoi = 42L),
    regexp = "sf or sfc object"
  )
})

# --- read_gssurgo_table() ----------------------------------------------------

test_that("read_gssurgo_table() errors on non-existent table or non-readable GDB", {
  # A fake empty directory either causes sf::st_layers() to fail with an
  # "Open failed" / "Could not list layers" message, or — if GDAL can read it
  # as an empty GDB — produces a "not found" message for the missing table.
  # Both outcomes mean the function fails loudly, which is what we test here.
  fake_gdb <- tempfile(pattern = "fake_gdb", fileext = ".gdb")
  dir.create(fake_gdb)
  on.exit(unlink(fake_gdb, recursive = TRUE))

  expect_error(
    read_gssurgo_table(fake_gdb, table_name = "nonexistent_table",
                       key_values = "1234"),
    regexp = "not found|Could not list layers|Open failed"
  )
})
