# seq_ID_assign
This tool is being implemented by WAPHL in order to create anonymous sequence identifiers to be linked to internal specimen ID's. The anonymous seq ID is used for upload of sequencing data to public repositories while the link to internal specimen identifiers remains confidential. 
This package also includes scripts to generate the metadata csv files necessary for uploads to NCBI. DIQA and MolEpi can query the DuckDB file using SQL.

# Pipeline overview
Running the pipeline will 
1. Download from BaseSpace samplesheets for all runs created in the last 10 days.
2. Query LIMS for all WA IDs in the samplesheet collection and pull sample metadata needed for NCBI.
3. Generate a unique, anonymous ID for each internal WA ID and write to a persistent DuckDB file. 
  *only one user may write to the DuckDB file at a time. If you try to write to it while it is already running, you will be alerted and asked to try again later!*
4. Export a sanitized metadata file to use for NCBI data submissions. 

# Quick Start
## Download requirements
To run this tool, you'll need to clone this repository and install the following:
- R (>=4.3.0)
- BaseSpace CLI https://launch.basespace.illumina.com/CLI/latest/amd64-windows/bs.exe and add to $PATH.
- jq https://github.com/jqlang/jq/releases/download/jq-1.8.1/jq-windows-amd64.exe and add to $PATH.
- Required R packages should install upon running the `main.sh` script. If not, try running `renv::restore()`.
## BaseSpace CLI setup
`bs auth`
## Create .Renviron file
Your .Renviron file must contain paths for the following:
STARLIMS_PATH=
DUCKDB_PATH=
LOCK_PATH=
SECURE_PATH=
EXPORT_PATH=
ACCESS_TOKEN=
API_SERVER=
*Save the file in the project root and add it to .gitignore.*
## Connect to internal VPN
You'll need to be on WADOH VPN to access LIMS and the secure drive. 

## Run from project root
`bash main.sh`


