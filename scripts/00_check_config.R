# 00_check_config.R
# Smoke-test entry point: load pipeline configuration helpers and verify the
# project environment is set up correctly. Run this from the project root:
#
#   Rscript scripts/00_check_config.R
#
# This script performs no data acquisition or analysis.

source("R/pipeline_config.R")
check_pipeline_config()

message("Script 00 complete: 0 records processed, 0 excluded, 0 unknown")
