# seq_ID_assign

## Overview
This tool is being implemented by WAPHL in order to create anonymous sequence identifiers to be linked to internal specimen ID's. The anonymous seq ID is used for upload of sequencing data to public repositories while the link to internal specimen identifiers remains confidential. 
This package also includes scripts to generate the metadata csv files necessary for uploads to NCBI. 

## Inputs 
Sequencing sample sheets are pulled from Illumina BaseSpace, from which the internal WA ID is extracted as well as a pathogen descriptor. 

## Anonymous ID generation 
Randomized sequencing ID's are generated and written to a DuckDB database file. Each time the tool is run the database file is searched to confirm the internal WA ID has not already been recorded. The database file is stored in a secure location where external stakeholders(Mol Epi, DIQA) are able to query the file with read-only access. 

## Sample metadata file creation
Sample metadata files necessary for upload to NCBI are generated and stored (in an s3 bucket? wherever makes sense for binfx)

## Requirements 
Currently the tool is being developed in R/Rstudio. User needs to be connected to internal VPN and have access to the secure location of the DuckDB file.
