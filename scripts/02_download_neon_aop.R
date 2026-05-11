# 02_download_neon_aop.R
# SKELETON — pipeline shape only; no executable logic yet.
#
# Intended purpose:
#   Download NEON Airborne Observation Platform (AOP) products for the Santa
#   Rita Experimental Range (NEON site SRER) using `neonUtilities`.
#
# Intended inputs:
#   - NEON_TOKEN read from environment (see .env.example)
#   - AOP product IDs and survey year(s) to be declared as named constants
#
# Intended outputs:
#   - data/raw/neon_aop/                 raw downloads (gitignored)
#   - data/snapshots/neon_aop_<date>.csv manifest (git-tracked)
#
# Pending scientific decisions (PI to confirm before implementing):
#   - Which AOP products (e.g. DP3.30015.001 CHM, DP3.30006.001 reflectance,
#     DP3.30026.001 vegetation indices, DP3.30012.001 LAI, etc.)
#   - Which survey year(s)
#   - Tile extent vs full-site mosaic

source("R/pipeline_config.R")
check_pipeline_config()

# TODO: implement download logic using neonUtilities::byTileAOP / byFileAOP

message("Script 02 complete: 0 records processed, 0 excluded, 0 unknown")
