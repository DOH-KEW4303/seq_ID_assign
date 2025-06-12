

#load libraries 
library(DBI)
library(odbc)  
library(dplyr)
library(readr)
library(tidyverse)
library(duckdb)

# Credentials
starLIMS_path <- Sys.getenv("STARLIMS_PATH")
db_info <- read_tsv(starLIMS_path)

#for now input a list of basespace seq ID's that will be used to query lims 
basespace_id <- read.csv("../tb_fetch0604.csv")
basespace_id <- basespace_id %>%
  mutate(wa_id = sub("-.*", "", bs_id))%>%
  select(-bs_id)#extract only WA id

# Connection
con <- DBI::dbConnect(odbc::odbc(),
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
    dbGetQuery(con, sql_query)
  })) %>%
  unnest(lims_info)

#add anon id col to dataframe



print(results)