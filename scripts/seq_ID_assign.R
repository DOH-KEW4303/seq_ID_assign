

#load libraries 
library(DBI)
library(odbc)  
library(dplyr)
library(readr)
library(tidyverse)
library(duckdb)

readRenviron("../.Renviron")  #load renviron file here

# Credentials
starLIMS_path <- Sys.getenv("STARLIMS_PATH")
db_info <- read_tsv(starLIMS_path)

#for now input a list of basespace seq ID's that will be used to query lims 
basespace_id <- read.csv("../tb_fetch0604.csv")
basespace_id <- basespace_id %>%
  mutate(wa_id = sub("-.*", "", bs_id))%>%
  select(-bs_id)#extract only WA id

# Connection
lims_con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server Native Client 11.0",
                      Server = db_info$server,
                      Database = db_info$database,
                      Trusted_connection = "yes",
                      ApplicationIntent = "ReadOnly",
                      timezone = Sys.timezone(),
                      timezone.out = Sys.timezone())

#query 
results <- basespace_id %>%
  mutate(lims_info = map(wa_id, function(id) {
    sql_query <- paste0(
      "SELECT TOP 1 SpecimenDateCollected, SpecimenSource, PatientAddressCounty, SubmitterName ",
      "FROM ", db_info$database, ".dbo.", db_info$table, " ",
      "WHERE PHLAccessionNumber = '", id, "'"
    )
    dbGetQuery(lims_con, sql_query)
  })) %>%
  unnest(lims_info)

# Get path from .Renviron
db_path <- Sys.getenv("DUCKDB_PATH")

# Create/connect to the DuckDB file and create table
assign_anon_ids <- function(results, db_path) {
  # Connect to duckdb (read-only = FALSE because we're inserting)
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  
  # Create table if it doesn't exist yet
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS anon_ids (
      wa_id TEXT PRIMARY KEY,
      anon_id TEXT,
      year_collected INTEGER
    )
  ")
  # Loop through results df and assign anon_id if not already in DB
  results<- results %>%
    rowwise() %>%
    mutate(
      anon_id = {
        existing_id <- dbGetQuery(con, paste0(
          "SELECT anon_id FROM anon_ids WHERE wa_id = '", wa_id, "'"
        ))
        
        if (nrow(existing_id) > 0) {
          existing_id$anon_id
        } else {
          # Generate new ID
          year <- year(as.Date(SpecimenDateCollected))
          repeat {
            rand_num <- sprintf("%06d", sample(1e6, 1))
            new_anon_id <- paste0("Mtb/Homo sapiens/USA/WA-PHL-", rand_num, "/", year)
            
            # Check if random ID is already used
            existing <- dbGetQuery(con, paste0(
              "SELECT 1 FROM anon_ids WHERE anon_id = '", new_anon_id, "'"
            ))
            
            if (nrow(existing) == 0) {
              # Insert into DuckDB
              dbExecute(con, "INSERT INTO anon_ids (wa_id, anon_id, year_collected) VALUES (?, ?, ?)", 
                        params = list(wa_id, new_anon_id, year))
              break
            }
          }
          new_anon_id
        }
      }
    ) %>%
    ungroup()
  
  dbDisconnect(con)
  return(results)
}

#call the function to assign ID's 
results <- assign_anon_ids(results, db_path)
view(results)



