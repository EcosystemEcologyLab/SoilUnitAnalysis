# 01_download_soilweb.R
#
# Purpose:
#   Download NRCS SSURGO soil unit polygons and tabular soil properties for
#   the Santa Rita Experimental Range from the Soil Data Access (SDA) web
#   service. Results are saved to data/snapshots/ssurgo/ with YAML provenance
#   sidecar files. Two QC logs are written to outputs/logs/.
#
# Inputs:
#   data/snapshots/srer_boundary/srer_boundary.shp  (git-tracked)
#
# Outputs (gitignored — regenerated each run):
#   data/snapshots/ssurgo/srer_mupolygons.gpkg
#   data/snapshots/ssurgo/srer_mupolygons.gpkg.provenance.yaml
#   data/snapshots/ssurgo/srer_properties.csv
#   data/snapshots/ssurgo/srer_properties.csv.provenance.yaml
#   outputs/logs/01_soilweb_unknowns_<UTC>.csv
#   outputs/logs/01_soilweb_exclusions_<UTC>.csv

source("R/pipeline_config.R")
check_pipeline_config()
source("R/soilweb_helpers.R")

library(soilDB)
library(sf)
library(dplyr)
library(readr)

# ── Scientific parameters ────────────────────────────────────────────────────

# DECISION: Output CRS — PROVISIONAL
# EPSG:26912 (NAD83 / UTM Zone 12N) matches the SRER boundary CRS confirmed
# from data/snapshots/srer_boundary/srer_boundary.prj and is the expected NEON
# AOP delivery CRS at SRER. Lock this in once 02_download_neon_aop.R confirms
# the AOP delivery CRS.
OUTPUT_CRS <- 26912L

# DECISION: Properties to retrieve — PI decision 2026-05-13
# Standard physical + chemical: texture (sand/silt/clay), bulk density (1/3-bar),
# available water capacity, organic matter, pH (1:1 H2O), CEC (pH 7), and
# electrical conductivity.
SSURGO_PROPERTIES <- c(
  "sandtotal_r", "silttotal_r", "claytotal_r",
  "dbthirdbar_r", "awc_r", "om_r",
  "ph1to1h2o_r", "cec7_r", "ec_r"
)

# DECISION: Aggregation method and depth interval — PROVISIONAL
# Weighted Average over 0–25 cm is a standard topsoil representation. The
# ecologically meaningful depth for SRER vegetation is unconfirmed. Confirm
# both values with PI before running script 04.
PROP_METHOD    <- "Weighted Average"
PROP_TOP_CM    <- 0L
PROP_BOTTOM_CM <- 25L

# ── Output directories ───────────────────────────────────────────────────────

dir.create("data/snapshots/ssurgo", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/logs",          showWarnings = FALSE, recursive = TRUE)

run_stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")

# ── Step 1: Load SRER boundary ───────────────────────────────────────────────
# No buffer — exact SRER boundary polygon (PI decision 2026-05-13).

message("01: loading SRER boundary ...")
srer_boundary <- load_srer_boundary()

# ── Step 2: Query SSURGO map unit polygons ───────────────────────────────────

message("01: querying SSURGO polygons from SDA ...")
mupolygons <- query_ssurgo_polygons(aoi = srer_boundary)

# Capture provenance attributes before st_transform drops them.
query_datetime_utc <- attr(mupolygons, "query_datetime_utc")
aoi_wkt            <- attr(mupolygons, "aoi_wkt")

message(sprintf("01: received %d map unit polygons.", nrow(mupolygons)))

# ── Step 3: Reproject to output CRS ─────────────────────────────────────────
# See DECISION: Output CRS above.

mupolygons_utm <- sf::st_transform(mupolygons, crs = OUTPUT_CRS)

# ── Step 4: Write polygon snapshot with provenance ──────────────────────────

poly_path <- "data/snapshots/ssurgo/srer_mupolygons.gpkg"

sf::st_write(mupolygons_utm, poly_path, delete_dsn = TRUE, quiet = TRUE)

write_provenance_sidecar(
  output_path   = poly_path,
  input_sources = list(
    srer_boundary      = "data/snapshots/srer_boundary/srer_boundary.shp",
    sda_product        = "SSURGO mupolygon via SDA_spatialQuery(geomIntersection = TRUE)",
    query_datetime_utc = query_datetime_utc,
    aoi_wkt            = aoi_wkt,
    output_crs_epsg    = OUTPUT_CRS
  ),
  notes = paste(
    "Output CRS EPSG:26912 is PROVISIONAL — lock in after 02_download_neon_aop.R",
    "confirms the NEON AOP delivery CRS at SRER."
  )
)

message(sprintf("01: wrote %s", poly_path))

# ── Steps 5–6: Query soil properties ─────────────────────────────────────────
# See DECISION: Aggregation method and depth interval above.

mukeys <- unique(as.character(mupolygons_utm$mukey))
message(sprintf(
  "01: querying %d properties for %d unique mukeys ...",
  length(SSURGO_PROPERTIES), length(mukeys)
))

soil_props <- query_ssurgo_properties(
  mukeys       = mukeys,
  properties   = SSURGO_PROPERTIES,
  method       = PROP_METHOD,
  top_depth    = PROP_TOP_CM,
  bottom_depth = PROP_BOTTOM_CM
)

message(sprintf("01: received property rows for %d mukeys.", nrow(soil_props)))

# ── Step 7: Write properties snapshot with provenance ────────────────────────

props_path <- "data/snapshots/ssurgo/srer_properties.csv"

readr::write_csv(soil_props, props_path)

write_provenance_sidecar(
  output_path   = props_path,
  input_sources = list(
    srer_mupolygons    = poly_path,
    properties         = SSURGO_PROPERTIES,
    aggregation_method = PROP_METHOD,
    depth_interval_cm  = paste0(PROP_TOP_CM, "-", PROP_BOTTOM_CM),
    soildb_version     = as.character(packageVersion("soilDB"))
  ),
  notes = paste(
    "PROVISIONAL: aggregation method ('Weighted Average') and depth interval",
    "(0-25 cm) have not been confirmed by PI. Do not use results from this file",
    "in downstream analysis until confirmed and this note is updated."
  )
)

message(sprintf("01: wrote %s", props_path))

# ── Step 8: QC logs ──────────────────────────────────────────────────────────

prop_mukeys       <- unique(as.character(soil_props$mukey))
in_geom_not_props <- setdiff(mukeys,      prop_mukeys)
in_props_not_geom <- setdiff(prop_mukeys, mukeys)

unknowns <- data.frame(mukey = character(0L), issue = character(0L))
if (length(in_geom_not_props) > 0L) {
  unknowns <- dplyr::bind_rows(unknowns, data.frame(
    mukey = in_geom_not_props,
    issue = "mukey present in geometry but absent from property query result"
  ))
}
if (length(in_props_not_geom) > 0L) {
  unknowns <- dplyr::bind_rows(unknowns, data.frame(
    mukey = in_props_not_geom,
    issue = "mukey present in property query result but absent from geometry"
  ))
}

unknowns_path <- file.path(
  "outputs/logs", paste0("01_soilweb_unknowns_", run_stamp, ".csv")
)
readr::write_csv(unknowns, unknowns_path)

exclusions_path <- file.path(
  "outputs/logs", paste0("01_soilweb_exclusions_", run_stamp, ".csv")
)
readr::write_csv(
  data.frame(mukey = character(0L), reason = character(0L), decided_by = character(0L)),
  exclusions_path
)

message(sprintf("01: %d unknowns logged to %s", nrow(unknowns), unknowns_path))
message(sprintf("01: 0 exclusions; schema-only log written to %s", exclusions_path))

# ── Completion ───────────────────────────────────────────────────────────────

message(sprintf(
  "Script 01 complete: %d mupolygons processed, 0 excluded, %d unknown",
  nrow(mupolygons_utm),
  nrow(unknowns)
))
