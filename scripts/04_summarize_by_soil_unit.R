# 04_summarize_by_soil_unit.R
# SKELETON — pipeline shape only; no executable logic yet.
#
# Intended purpose:
#   Summarise the trait stack from script 03 within NRCS soil unit polygons
#   from script 01 to produce one summary row per soil unit.
#
# Intended inputs:
#   - data/raw/soilweb/ or data/processed/soilweb/   NRCS polygons
#   - data/processed/traits/                         trait stack
#
# Intended outputs:
#   - outputs/soil_unit_summary.csv  one row per soil unit (gitignored)
#   - outputs/session_info.txt       required per principles file
#
# Pending scientific decisions (PI to confirm before implementing):
#   - Summary statistics (mean, median, quantiles, etc.)
#   - Minimum pixel coverage threshold per polygon
#   - Whether to weight by polygon area or pixel count

source("R/pipeline_config.R")
check_pipeline_config()

# TODO: implement zonal summary

message("Script 04 complete: 0 records processed, 0 excluded, 0 unknown")
