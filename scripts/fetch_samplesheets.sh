#!/bin/bash

PS4='+ '  # Prevent verbose __git_ps1 noise


source "../secrets/env_vars.sh"
echo "Token: $ACCESS_TOKEN"
echo "Server: $API_SERVER"


# Create output folder if it doesn't exist
output_dir="../samplesheets"

# Fetch list of sequencing runs from the last 10 days (adjust as needed)
run_ids=$(bs list run --access-token "$ACCESS_TOKEN" --api-server "$API_SERVER" -f json | \
  jq -r --arg date_cutoff "$(date -d '10 days ago' +%Y-%m-%d)" \
  '.[] | select(.DateCreated >= $date_cutoff) | .Id')


# Download sample sheets for each run
for run_id in $run_ids; do
  echo "Fetching sample sheet for run: $run_id"
  bs run download -i "$run_id" \
      --access-token "$ACCESS_TOKEN" \
      --api-server "$API_SERVER" \
      -o "$output_dir" \
      --extension=csv
   #Rename the SampleSheet.csv 
   if [ -f "$output_dir/SampleSheet.csv" ]; then
    mv "$output_dir/SampleSheet.csv" "$output_dir/SampleSheet_${run_id}.csv"
   else
    echo "No samplesheet.csv retrieved for run $run_id"
   fi  
done
     

    

