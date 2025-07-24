#!/bin/bash
#
#Description: Fetches samplesheets for runs in BaseSpace create <10 days ago. Queries starLIMS for each WA ID, extracting metadata for downstream use. Assigns a unique, anonymous identifier for each WA ID and writes to a DuckDB file on the Y drive. Exports sanitized metadata for NCBI upload.
#
#Author: K.Waterman

### Config logging ###
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_DIR="./main_logs"
LOG_FILE="${LOG_DIR}/pipeline_run_${TIMESTAMP}.log"
mkdir -p "$LOG_DIR"

### Load R env variables from .Renviron file ###
set -a
source ./.Renviron
set +a

### Load R libraries with renv ##
Rscript -e "if (!requireNamespace('renv', quietly = TRUE))install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore()"
echo "Required R libraries successfully loaded" | tee -a "$LOG_FILE"

### Download samplesheet.csv files from BaseSpace ###
bash ./scripts/fetch_samplesheets.sh
echo "Samplesheet.csv retrieved from BaseSpace" | tee -a "$LOG_FILE"

### Run LIMS query ###
Rscript ./scripts/lims_query.R 
echo "LIMS query completed" | tee -a "$LOG_FILE"

### Assign anon ID and write to DuckDB ###
Rscript ./scripts/anon_ID_assign.R
echo "Anonymous ID's assigned, check script log for errors" | tee -a "$LOG_FILE"

echo "New anon. ID's written to DuckDB file. Done!" | tee -a "$LOG_FILE"

### Sanitized (no internal WA ID) metadata file exported to... ###