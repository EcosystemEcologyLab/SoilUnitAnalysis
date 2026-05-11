# 03_extract_traits.R
# SKELETON — pipeline shape only; no executable logic yet.
#
# Intended purpose:
#   Extract per-pixel vegetation trait surfaces from NEON AOP rasters and
#   produce a clean, project-CRS trait stack covering the AOI.
#
# Intended inputs:
#   - data/raw/neon_aop/  raw AOP rasters from script 02
#
# Intended outputs:
#   - data/processed/traits/  cleaned trait stack (gitignored)
#
# Pending scientific decisions (PI to confirm before implementing):
#   - Which traits to derive (canopy height, fractional cover, NDVI, etc.)
#   - QC masking thresholds (cloud, shadow, edge tiles)
#   - Target CRS and resolution for the trait stack

source("R/pipeline_config.R")
check_pipeline_config()

# TODO: implement trait extraction

message("Script 03 complete: 0 records processed, 0 excluded, 0 unknown")
