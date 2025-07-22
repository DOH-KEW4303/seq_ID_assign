#!/bin/bash

PS4='+ '  # Prevent verbose __git_ps1 noise


source "../secrets/env_vars.sh"
echo "Token: $ACCESS_TOKEN"
echo "Server: $API_SERVER"


# Create output folder if it doesn't exist
mkdir -p samplesheets

# Fetch sequencing runs from the last 10 days (adjust as needed)
run_ids=$(bs list run --access-token "$ACCESS_TOKEN" --api-server "$API_SERVER" -f json | \
  jq -r --arg date_cutoff "$(date -d '10 days ago' +%Y-%m-%d)" \
  '.[] | select(.DateCreated >= $date_cutoff) | .Id')


# Download sample sheets for each run
for run_id in $run_ids; do
  echo "Fetching sample sheet for run: $run_id"

  #file_id=$(bs list run --access-token "$ACCESS_TOKEN" --api-server "$API_SERVER" -f json | \
    #jq -r '.[] | select(.Name == "SampleSheet.csv") | .Id')

  #if [ -n "$file_id" ]; then
  bs run download -i "$run_id" \
      --access-token "$ACCESS_TOKEN" \
      --api-server "$API_SERVER" \
      -o "./samplesheets/" \
      --extension=csv
  # Rename the SampleSheet.csv 
  if [ -f "./samplesheets/SampleSheet.csv" ]; then
    mv "./samplesheets/SampleSheet.csv" "./samplesheets/SampleSheet_${run_id}.csv"
  else
    echo "No SampleSheet.csv found for run $run_id"
  fi
done
     
  #else
    #echo "No SampleSheet.csv found for run $run_id"
    

