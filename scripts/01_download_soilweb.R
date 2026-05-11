# 01_download_soilweb.R
# SKELETON — pipeline shape only; no executable logic yet.
#
# Intended purpose:
#   Download NRCS soil unit polygons and tabular descriptions for the Santa
#   Rita Experimental Range from SoilWeb / Web Soil Survey.
#
# Intended inputs:
#   - Area-of-interest bounding box for the Santa Rita Experimental Range
#     (to be declared as a named constant once finalised).
#
# Intended outputs:
#   - data/raw/soilweb/         raw downloads (gitignored)
#   - data/snapshots/soilweb_<date>.csv  manifest (git-tracked)
#
# Pending scientific decisions (PI to confirm before implementing):
#   - Exact AOI extent and CRS
#   - SoilWeb product(s) to pull (SSURGO map units vs STATSGO, attributes)
#   - Snapshot manifest schema

source("R/pipeline_config.R")
check_pipeline_config()

# TODO: implement download logic

message("Script 01 complete: 0 records processed, 0 excluded, 0 unknown")
