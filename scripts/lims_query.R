

#load libraries 
library(DBI)
library(odbc)  
library(tidyverse)
library(glue)

readRenviron("../.Renviron")  #load renviron file here

# Credentials
starLIMS_path <- Sys.getenv("STARLIMS_PATH")
db_info <- read_tsv(starLIMS_path)
secure_path <- Sys.getenv("SECURE_PATH")

#read lines to get the data section only of wonky samplesheet formatting
read_samplesheet_data <- function(file_path) {
  lines <- readLines(file_path)
  data_start <- grep("^\\[Data\\]", lines)
  if (length(data_start) == 0) {
    warning(paste("No [Data] section found in", file_path))
    return(NULL)
  }
  
  data_lines <- lines[(data_start + 1):length(lines)]
  read.csv(text = paste(lines[(data_start + 1):length(lines)], collapse = "\n"), stringsAsFactors = FALSE)
}


samplesheet_dir <- "../samplesheets"
samplesheet_files <- list.files(
  path = samplesheet_dir, 
  pattern = "^SampleSheet_.*\\.csv$", 
  full.names = TRUE
  )
samplesheets.df <-do.call(
  rbind, 
  lapply(samplesheet_files, read_samplesheet_data)
  )
head(samplesheets.df)

samplesheets.df <- samplesheets.df%>%
  mutate(wa_id = sub("-.*", "", Sample_ID))%>%
  select(wa_id, Description)

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
results <- samplesheets.df %>%
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


