#!/bin/bash
#
#Description: Queries starLIMS for each WA ID, extracting metadata for downstream use. Assigns a unique, anonymous identifier for each WA ID and writes to a DuckDB file on the Y drive. Exports sanitized metadata for NCBI upload.
#
#Author: K.Waterman

set -euo pipefail 

### Config logging ###
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_DIR="./main_logs"
LOG_FILE="${LOG_DIR}/pipeline_run_${TIMESTAMP}.log"
mkdir -p "$LOG_DIR"

# Redirect stdout and stderr to log file as well as console
exec > >(tee -a "$LOG_FILE") 2>&1

### Load R env variables from .Renviron file ###
echo "Loading env variables..."
set -a
source ./.Renviron
set +a
echo "Env variable loaded successfully"


### Load R libraries with renv ##
echo "Restoring R env with renv"
Rscript -e "if (!requireNamespace('renv', quietly = TRUE))install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore()"
echo "Required R libraries successfully loaded" 

### Run LIMS query ###
echo "Running LIMS query..."
Rscript ./scripts/lims_query.R 
echo "LIMS query completed" 

### Assign anon ID and write to DuckDB ###
echo "Assigning anonymous ID's..."
Rscript ./scripts/anon_ID_assign.R
echo "Anonymous ID's assigned, check script log for any issues"

echo "New anon. ID's written to DuckDB file. Done!" 

### Sanitized (no internal WA ID) metadata file exported to... ###