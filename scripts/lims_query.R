

#load libraries 
library(DBI)
library(odbc)  
library(dplyr)
library(readr)
library(tidyverse)
library(duckdb)
library(purrr)
library(glue)

readRenviron("../.Renviron")  #load renviron file here

# Credentials
starLIMS_path <- Sys.getenv("STARLIMS_PATH")
db_info <- read_tsv(starLIMS_path)
secure_path <- Sys.getenv("SECURE_PATH")

#for now input a list of basespace seq ID's that will be used to query lims 
basespace_id <- read.csv("../bs_IDs.csv")
basespace_id <- basespace_id %>%
  mutate(wa_id = sub("-.*", "", bs_id))%>%
  select(wa_id, descriptor)

# Connection
lims_con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server Native Client 11.0",
                      Server = db_info$server,
                      Database = db_info$database,
                      Trusted_connection = "yes",
                      ApplicationIntent = "ReadOnly",
                      timezone = Sys.timezone(),
                      timezone.out = Sys.timezone())

# code block to perform a search across all tables for specific WA ID
# source("search_tables.R")
# wa_id <- "WA0123456"
# results <- search_lims_tables(wa_id)
# print(results)

#query 
results <- basespace_id %>%
  mutate(lims_info = map(wa_id, function(id) {
    sql_query <- paste0(
      "SELECT TOP 1 SpecimenDateCollected, SpecimenSource, PatientAddressCountry, PatientAddressState, PatientAddressCounty, SubmitterName ",
      "FROM ", db_info$database, ".dbo.", db_info$table, " ",
      "WHERE PHLAccessionNumber = '", id, "'"
    )
    dbGetQuery(lims_con, sql_query)
  })) %>%
  unnest(lims_info)
results<- results %>%
  mutate(SpecimenDateCollected = as.Date(SpecimenDateCollected)) %>%
  rename("collection_date" = SpecimenDateCollected,
         "isolation_source" = SpecimenSource,
         "county" = PatientAddressCounty,
         "state" = PatientAddressState,
         "country" = PatientAddressCountry,
         "collected_by" = SubmitterName)

print(results)

# Save results for use in duckDB and metadata scripts 
saveRDS(results, file = file.path(secure_path, "lims_query_results.rds"))


