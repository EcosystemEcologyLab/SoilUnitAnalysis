# 01_download_gssurgo.R
#
# Purpose:
#   Ingest the NRCS gSSURGO Arizona FileGeodatabase, extract MUPOLYGON
#   polygons and the MapunitRaster_10m grid for the Santa Rita Experimental
#   Range (SRER), pull Valu1 (pre-aggregated multi-depth properties) and raw
#   chorizon data, compute a provisional component-weighted depth-integrated
#   average for nine soil properties over 0-25 cm, and write all outputs to
#   data/snapshots/ssurgo/ with YAML provenance sidecars.
#
# Inputs:
#   data/raw/gSSURGO_AZ.zip                         (gitignored — obtain from NRCS)
#   data/snapshots/srer_boundary/srer_boundary.shp  (git-tracked)
#
# Outputs (gitignored — regenerated each run):
#   data/snapshots/ssurgo/srer_mupolygon.gpkg
#   data/snapshots/ssurgo/srer_mupolygon.gpkg.provenance.yaml
#   data/snapshots/ssurgo/srer_raster.tif
#   data/snapshots/ssurgo/srer_raster.tif.provenance.yaml
#   data/snapshots/ssurgo/srer_valu1.csv
#   data/snapshots/ssurgo/srer_valu1.csv.provenance.yaml
#   data/snapshots/ssurgo/srer_chorizon_aggregated.csv
#   data/snapshots/ssurgo/srer_chorizon_aggregated.csv.provenance.yaml
#   outputs/logs/01_gssurgo_mukey_mismatch_<UTC>.csv
#   outputs/logs/01_gssurgo_chorizon_na_<UTC>.csv
#   outputs/logs/01_gssurgo_unknowns_<UTC>.csv
#   outputs/logs/01_gssurgo_exclusions_<UTC>.csv

source("R/pipeline_config.R")
check_pipeline_config()
source("R/ssurgo_helpers.R")
source("R/gssurgo_helpers.R")
library(sf)
library(terra)
library(dplyr)
library(readr)
library(digest)

# ── Scientific parameters ────────────────────────────────────────────────────

# DECISION: Nine properties to aggregate from chorizon — PI decision 2026-05-13
# Same set as the prior SDA pipeline (decision unchanged; only the data source
# has changed to gSSURGO). Standard physical + chemical: texture (sand/silt/
# clay), bulk density (1/3-bar), available water capacity, organic matter,
# pH (1:1 H2O), CEC (pH 7), and electrical conductivity.
SSURGO_PROPERTIES <- c(
  "sandtotal_r", "silttotal_r", "claytotal_r",
  "dbthirdbar_r", "awc_r", "om_r",
  "ph1to1h2o_r", "cec7_r", "ec_r"
)

# DECISION: Aggregation depth interval — PROVISIONAL
# 0-25 cm is a standard topsoil representation carried over from the SDA
# pipeline. The ecologically meaningful depth for SRER vegetation is
# unconfirmed. Confirm this interval with PI before running script 04.
PROP_TOP_CM    <- 0L
PROP_BOTTOM_CM <- 25L

# DECISION: Mukey cross-check tolerance — technical threshold
# Differences of <=5 mukeys between the polygon and raster representations at
# the AOI boundary are expected edge effects; larger differences warrant
# investigation.
MUKEY_MISMATCH_WARN_THRESHOLD <- 5L

# gSSURGO_AZ.zip as published by NRCS; latest archive file timestamp 2025-11-20
GSSURGO_ZIP_SHA256 <- "d9f71fff10833851976eac2eefb1ae801df51ab2194ce2887b44f97b86bc6d04"
GSSURGO_ZIP_PATH   <- "data/raw/gSSURGO_AZ.zip"
GSSURGO_DEST_DIR   <- "data/raw"
GSSURGO_PUB_DATE   <- "2025-11-20"

# ── Output directories ───────────────────────────────────────────────────────

dir.create("data/snapshots/ssurgo", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/logs",          showWarnings = FALSE, recursive = TRUE)

run_stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")

# ── Step 1: Verify the gSSURGO ZIP ──────────────────────────────────────────

message("01: verifying gSSURGO ZIP ...")
verify_gssurgo_zip(
  path            = GSSURGO_ZIP_PATH,
  expected_sha256 = GSSURGO_ZIP_SHA256
)
message(sprintf("01: SHA-256 verified: %s", GSSURGO_ZIP_PATH))

# ── Step 2: Extract the ZIP ──────────────────────────────────────────────────

message("01: extracting gSSURGO ZIP (idempotent) ...")
gdb_path <- extract_gssurgo_zip(
  zip_path = GSSURGO_ZIP_PATH,
  dest_dir = GSSURGO_DEST_DIR
)
message(sprintf("01: GDB available at: %s", gdb_path))

# ── Step 3: Load SRER boundary and reproject to EPSG:5070 ───────────────────
# EPSG:5070 is the gSSURGO native CRS (CONUS Albers Equal Area). Only an
# in-memory copy is reprojected; the on-disk boundary in EPSG:26912 is
# unchanged.

message("01: loading SRER boundary ...")
srer_boundary      <- load_srer_boundary()
srer_boundary_5070 <- sf::st_transform(srer_boundary, crs = 5070L)

# ── Step 4: Read MUPOLYGON polygons ─────────────────────────────────────────

message("01: reading MUPOLYGON from GDB ...")
srer_mupolygon <- read_gssurgo_mupolygon(gdb_path, aoi = srer_boundary_5070)

poly_mukeys <- unique(as.character(srer_mupolygon$mukey))
message(sprintf(
  "01: %d MUPOLYGON features, %d unique mukeys",
  nrow(srer_mupolygon), length(poly_mukeys)
))

# ── Step 5: Read MapunitRaster_10m ──────────────────────────────────────────

message("01: reading MapunitRaster_10m from GDB ...")
srer_raster <- read_gssurgo_raster(gdb_path, aoi = srer_boundary_5070)

rast_vals   <- terra::values(srer_raster, na.rm = TRUE)
rast_mukeys <- unique(as.character(rast_vals[, 1L]))
message(sprintf(
  "01: raster cells %d x %d m, %d unique non-NA mukeys",
  terra::xres(srer_raster), terra::yres(srer_raster), length(rast_mukeys)
))

# ── Step 6: Cross-check mukey sets ──────────────────────────────────────────

only_in_poly  <- setdiff(poly_mukeys,  rast_mukeys)
only_in_rast  <- setdiff(rast_mukeys,  poly_mukeys)
all_diff      <- union(only_in_poly, only_in_rast)
n_diff        <- length(all_diff)

mismatch_df <- data.frame(
  mukey             = all_diff,
  present_in_polygons = all_diff %in% poly_mukeys,
  present_in_raster   = all_diff %in% rast_mukeys,
  stringsAsFactors  = FALSE
)

mismatch_path <- file.path(
  "outputs/logs", paste0("01_gssurgo_mukey_mismatch_", run_stamp, ".csv")
)
readr::write_csv(mismatch_df, mismatch_path)
message(sprintf(
  "01: mukey cross-check: %d mukeys differ between polygons and raster; log: %s",
  n_diff, mismatch_path
))

if (n_diff > MUKEY_MISMATCH_WARN_THRESHOLD) {
  warning(sprintf(
    "Mukey set mismatch (%d mukeys) exceeds threshold of %d. Inspect %s.",
    n_diff, MUKEY_MISMATCH_WARN_THRESHOLD, mismatch_path
  ), call. = FALSE)
}

# DECISION: mukey set for downstream analysis — PI decision 2026-05-14
# Use the INTERSECTION of polygon and raster mukey sets. Union was rejected
# because it admits mukeys with only polygon or only raster coverage: raster-
# only mukeys may represent a single cell of clipping noise and are not
# meaningful analysis units; polygon-only mukeys have no gridded mukey values
# and cannot be matched to NEON AOP raster comparisons downstream.
# Intersection ensures every retained mukey has both vector polygon extent and
# raster pixel coverage within the SRER AOI.
# Mukeys present in one layer but not both are logged above in the mismatch CSV
# and will appear in the unknowns log below.
srer_mukeys <- intersect(poly_mukeys, rast_mukeys)

# ── Step 7: Read Valu1 table ─────────────────────────────────────────────────
# Valu1 contains pre-aggregated multi-depth AWS and SOC stocks — the primary
# scientific reason for using gSSURGO over the SDA API.

message(sprintf("01: reading Valu1 for %d SRER mukeys ...", length(srer_mukeys)))
valu1_srer <- read_gssurgo_table(
  gdb_path    = gdb_path,
  table_name  = "Valu1",
  key_values  = srer_mukeys,
  key_col     = "mukey"
)
message(sprintf("01: Valu1 — %d rows, %d columns", nrow(valu1_srer), ncol(valu1_srer)))

# ── Step 8: Read component and chorizon tables ───────────────────────────────
# Explicit two-step join: mukey → cokey (via component), cokey → chkey
# (via chorizon). Join logic is visible here per pipeline conventions.

message("01: reading component table ...")
component_srer <- read_gssurgo_table(
  gdb_path   = gdb_path,
  table_name = "component",
  key_values = srer_mukeys,
  key_col    = "mukey"
)
srer_cokeys <- unique(as.character(component_srer$cokey))
message(sprintf(
  "01: component — %d rows, %d unique cokeys",
  nrow(component_srer), length(srer_cokeys)
))

message("01: reading chorizon table ...")
chorizon_srer <- read_gssurgo_table(
  gdb_path   = gdb_path,
  table_name = "chorizon",
  key_values = srer_cokeys,
  key_col    = "cokey"
)
message(sprintf("01: chorizon — %d rows", nrow(chorizon_srer)))

# Explicit join: chorizon.cokey -> component.cokey -> component.mukey
chorizon_srer <- chorizon_srer |>
  dplyr::left_join(
    component_srer |> dplyr::select(cokey, mukey, comppct_r),
    by = "cokey"
  )

# ── Step 9: Aggregate chorizon over 0–25 cm — PROVISIONAL ──────────────────
#
# DECISION: depth interval and aggregation method — PROVISIONAL
# Aggregation: component-weighted average of depth-weighted property means
# over the 0–25 cm interval.
# Depth weighting:  weight = horizon thickness within [0, 25] cm window.
# Component weight: weight = comppct_r (component percent of map unit).
# Normalization:    divide by the sum of applicable weights.
# This interval and method are PROVISIONAL — confirm with PI before script 04
# is run. Do not remove this PROVISIONAL flag until PI confirmation.

# Clip horizons to the 0–25 cm window and compute horizon thickness
chorizon_0_25 <- chorizon_srer |>
  dplyr::mutate(
    hz_overlap_top    = pmax(hzdept_r, PROP_TOP_CM),
    hz_overlap_bottom = pmin(hzdepb_r, PROP_BOTTOM_CM),
    hz_thickness      = hz_overlap_bottom - hz_overlap_top
  ) |>
  dplyr::filter(hz_thickness > 0)

# Log NA property values within the 0–25 cm window before aggregation
na_rows_list <- lapply(SSURGO_PROPERTIES, function(prop) {
  rows <- chorizon_0_25[is.na(chorizon_0_25[[prop]]), ]
  if (nrow(rows) == 0L) return(NULL)
  data.frame(
    mukey    = as.character(rows$mukey),
    cokey    = as.character(rows$cokey),
    chkey    = as.character(rows$chkey),
    property = prop,
    hzdept_r = rows$hzdept_r,
    hzdepb_r = rows$hzdepb_r,
    stringsAsFactors = FALSE
  )
})
na_log_df <- do.call(rbind, Filter(Negate(is.null), na_rows_list))
if (is.null(na_log_df)) {
  na_log_df <- data.frame(
    mukey    = character(0L),
    cokey    = character(0L),
    chkey    = character(0L),
    property = character(0L),
    hzdept_r = integer(0L),
    hzdepb_r = integer(0L)
  )
}

chorizon_na_path <- file.path(
  "outputs/logs", paste0("01_gssurgo_chorizon_na_", run_stamp, ".csv")
)
readr::write_csv(na_log_df, chorizon_na_path)
message(sprintf(
  "01: %d NA property-horizon combinations logged to %s",
  nrow(na_log_df), chorizon_na_path
))

# Helper: weighted average (returns NA if no valid observations)
.wtd_mean <- function(values, weights) {
  valid <- !is.na(values) & !is.na(weights) & weights > 0
  if (!any(valid)) return(NA_real_)
  sum(values[valid] * weights[valid]) / sum(weights[valid])
}

# Step 9a: depth-weighted average per component (cokey) per property
comp_avg_list <- lapply(SSURGO_PROPERTIES, function(prop) {
  chorizon_0_25 |>
    dplyr::filter(!is.na(.data[[prop]])) |>
    dplyr::group_by(cokey) |>
    dplyr::summarise(
      !!prop := .wtd_mean(.data[[prop]], hz_thickness),
      .groups = "drop"
    )
})

comp_avg <- Reduce(
  function(a, b) dplyr::left_join(a, b, by = "cokey"),
  comp_avg_list,
  init = component_srer |> dplyr::select(cokey, mukey, comppct_r)
)

# Step 9b: component-weighted average per mukey
chorizon_agg <- comp_avg |>
  dplyr::group_by(mukey) |>
  dplyr::summarise(
    sandtotal_r  = .wtd_mean(sandtotal_r,  comppct_r),
    silttotal_r  = .wtd_mean(silttotal_r,  comppct_r),
    claytotal_r  = .wtd_mean(claytotal_r,  comppct_r),
    dbthirdbar_r = .wtd_mean(dbthirdbar_r, comppct_r),
    awc_r        = .wtd_mean(awc_r,        comppct_r),
    om_r         = .wtd_mean(om_r,         comppct_r),
    ph1to1h2o_r  = .wtd_mean(ph1to1h2o_r,  comppct_r),
    cec7_r       = .wtd_mean(cec7_r,       comppct_r),
    ec_r         = .wtd_mean(ec_r,         comppct_r),
    .groups      = "drop"
  )

message(sprintf(
  "01: chorizon aggregated — %d mukeys, depth interval %d–%d cm (PROVISIONAL)",
  nrow(chorizon_agg), PROP_TOP_CM, PROP_BOTTOM_CM
))

# ── Step 10: Write outputs with provenance sidecars ─────────────────────────

provenance_notes_provisional <- paste(
  "PROVISIONAL: aggregation method (component-weighted average of depth-weighted",
  "property means) and depth interval (0-25 cm) have not been confirmed by PI.",
  "Do not use chorizon-derived outputs in downstream analysis until confirmed",
  "and this PROVISIONAL flag is removed."
)

gssurgo_input_sources <- list(
  gssurgo_zip      = basename(GSSURGO_ZIP_PATH),
  gssurgo_sha256   = GSSURGO_ZIP_SHA256,
  gssurgo_pub_date = GSSURGO_PUB_DATE,
  srer_boundary    = "data/snapshots/srer_boundary/srer_boundary.shp"
)

# 10a: MUPOLYGON vector
poly_path <- "data/snapshots/ssurgo/srer_mupolygon.gpkg"
sf::st_write(srer_mupolygon, poly_path, delete_dsn = TRUE, quiet = TRUE)
write_provenance_sidecar(
  output_path   = poly_path,
  input_sources = c(gssurgo_input_sources, list(
    layer       = "MUPOLYGON",
    output_crs  = "EPSG:5070"
  )),
  notes = "Vector polygons in EPSG:5070 (gSSURGO native CRS). Downstream analysis uses the intersection of polygon and raster mukey sets (PI decision 2026-05-14); mukeys present only in polygons are logged in the mismatch and unknowns logs."
)
message(sprintf("01: wrote %s", poly_path))

# 10b: MapunitRaster_10m
rast_path <- "data/snapshots/ssurgo/srer_raster.tif"
terra::writeRaster(srer_raster, rast_path, overwrite = TRUE)
write_provenance_sidecar(
  output_path   = rast_path,
  input_sources = c(gssurgo_input_sources, list(
    layer      = "MapunitRaster_10m",
    output_crs = "EPSG:5070",
    resolution = "10m"
  )),
  notes = "Mukey raster in EPSG:5070 (gSSURGO native CRS), 10 m resolution. Downstream analysis uses the intersection of polygon and raster mukey sets (PI decision 2026-05-14); mukeys present only in the raster are logged in the mismatch and unknowns logs."
)
message(sprintf("01: wrote %s", rast_path))

# 10c: Valu1 table (full — all columns kept)
valu1_path <- "data/snapshots/ssurgo/srer_valu1.csv"
readr::write_csv(valu1_srer, valu1_path)
write_provenance_sidecar(
  output_path   = valu1_path,
  input_sources = c(gssurgo_input_sources, list(layer = "Valu1")),
  notes         = "All Valu1 columns retained. Valu1 contains pre-aggregated AWS and SOC stocks at multiple depth intervals (0-25, 0-50, 0-100, 0-150, 0-999 cm)."
)
message(sprintf("01: wrote %s", valu1_path))

# 10d: Aggregated chorizon properties
chorizon_agg_path <- "data/snapshots/ssurgo/srer_chorizon_aggregated.csv"
readr::write_csv(chorizon_agg, chorizon_agg_path)
write_provenance_sidecar(
  output_path   = chorizon_agg_path,
  input_sources = c(gssurgo_input_sources, list(
    layers             = "component + chorizon",
    properties         = paste(SSURGO_PROPERTIES, collapse = ", "),
    aggregation_method = "component-weighted average of depth-weighted property means",
    depth_interval_cm  = paste0(PROP_TOP_CM, "-", PROP_BOTTOM_CM)
  )),
  notes = provenance_notes_provisional
)
message(sprintf("01: wrote %s", chorizon_agg_path))

# ── Step 11: QC logs ─────────────────────────────────────────────────────────

valu1_mukeys    <- unique(as.character(valu1_srer$mukey))
comp_mukeys     <- unique(as.character(component_srer$mukey))

unknowns <- data.frame(
  mukey = character(0L),
  issue = character(0L),
  stringsAsFactors = FALSE
)

# Mukeys in raster but not in Valu1
rast_not_valu1 <- setdiff(rast_mukeys, valu1_mukeys)
if (length(rast_not_valu1) > 0L) {
  unknowns <- dplyr::bind_rows(unknowns, data.frame(
    mukey = rast_not_valu1,
    issue = "mukey present in raster but absent from Valu1"
  ))
}

# Mukeys in MUPOLYGON but not in component
poly_not_comp <- setdiff(poly_mukeys, comp_mukeys)
if (length(poly_not_comp) > 0L) {
  unknowns <- dplyr::bind_rows(unknowns, data.frame(
    mukey = poly_not_comp,
    issue = "mukey present in MUPOLYGON but absent from component table"
  ))
}

# Mukeys in raster but not in component
rast_not_comp <- setdiff(rast_mukeys, comp_mukeys)
if (length(rast_not_comp) > 0L) {
  unknowns <- dplyr::bind_rows(unknowns, data.frame(
    mukey = rast_not_comp,
    issue = "mukey present in raster but absent from component table"
  ))
}

unknowns_path <- file.path(
  "outputs/logs", paste0("01_gssurgo_unknowns_", run_stamp, ".csv")
)
readr::write_csv(unknowns, unknowns_path)

exclusions_path <- file.path(
  "outputs/logs", paste0("01_gssurgo_exclusions_", run_stamp, ".csv")
)
readr::write_csv(
  data.frame(
    mukey      = character(0L),
    reason     = character(0L),
    decided_by = character(0L)
  ),
  exclusions_path
)

message(sprintf("01: %d unknowns logged to %s", nrow(unknowns), unknowns_path))
message(sprintf("01: 0 exclusions; schema-only log written to %s", exclusions_path))

# ── Completion ───────────────────────────────────────────────────────────────

message(sprintf(
  "Script 01 complete: %d mupolygons, %d raster mukeys, 0 excluded, %d unknown",
  nrow(srer_mupolygon),
  length(rast_mukeys),
  nrow(unknowns)
))
