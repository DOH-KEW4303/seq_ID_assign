### You will need a .Renviron file containing starlims path, database info, server info etc. Script will extract internal ID from the Basespace samplesheet (along with the pathogen descriptor), confirm new samples (not already in DuckDB file), query LIMS for 
the required metadata. Query results are saved and will be exported to a csv file with a later script ###

#load libraries 
library(DBI)
library(odbc)  
library(tidyverse)
library(glue)
library(fs)
library(duckdb)

readRenviron("../.Renviron")  #load renviron file here

# Configs (including multi tables)
starLIMS_path <- Sys.getenv("STARLIMS_PATH")
secure_path <- Sys.getenv("SECURE_PATH")
lims_common <- Sys.getenv("TABLE_COMMON")
lims_micro <- Sys.getenv("TABLE_MICRO")
lims_arbo <- Sys.getenv("TABLE_ARBO")
#lims_flu <-Sys.getenv("TABLE_FLU")
database <- Sys.getenv("DATABASE")
server <- Sys.getenv("SERVER")


esc<- function(x) gsub("'", "''", x)


#read lines to get the data section only of wonky samplesheet formatting
read_samplesheet_data <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  
  data_start <- grep("^\\s*\\[Data\\]\\s*(,.*)?$", lines, ignore.case = TRUE)
  if (length(data_start) == 0) return(NULL) #skip new samplsheets for now
  
  read.csv(text = paste(lines[(data_start[1] + 1):length(lines)], collapse = "\n"),
           stringsAsFactors = FALSE, 
           check.names = FALSE
           )
}

samplesheet_dir <- "samplesheets"
samplesheet_files <- list.files(
  path = samplesheet_dir, 
  pattern = "^SampleSheet.*\\.csv$", 
  full.names = TRUE
  )
print(samplesheet_files)
lines <- readLines(samplesheet_files[1], warn = FALSE)
lines[grepl("^\\[", lines)][1:30]


samplesheet_list <- lapply(samplesheet_files, read_samplesheet_data)
samplesheet_list <- Filter(Negate(is.null), samplesheet_list)

if (length(samplesheet_list) == 0) {
  stop("No old-format SampleSheets with [Data] found.")
}

samplesheets.df <- dplyr::bind_rows(samplesheet_list)

samplesheets.df <- samplesheets.df %>%
  mutate(
    wa_id = str_extract(Sample_ID, "WA\\d+" ))%>%
  filter(!is.na(wa_id) & wa_id != "") %>%
  distinct(wa_id, Description)

norm_id <- function(x) toupper(trimws(x))
ids_norm <- samplesheets.df %>%
  transmute(id_norm = norm_id(wa_id)) %>%
  distinct()


# Connect to DuckDB and fetch existing wa_ids
run_env <- Sys.getenv("RUN_ENV", unset = "DEV")
db_path <- if (run_env == "PROD") Sys.getenv("DUCKDB_PATH_PROD") else Sys.getenv("DUCKDB_PATH_DEV")

con_duck <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
on.exit(DBI::dbDisconnect(con_duck), add = TRUE)

existing_wa <- DBI::dbGetQuery(con_duck, "SELECT wa_id FROM anon_ids") %>%
  mutate(wa_id = norm_id(wa_id)) %>%
  distinct()

# Keep only WA IDs not already in duckdb
ids_norm_new <- ids_norm %>%
  anti_join(existing_wa, by = c("id_norm" = "wa_id"))

message("Total WA IDs in samplesheets: ", nrow(ids_norm))
message("New WA IDs not in DuckDB: ", nrow(ids_norm_new))

ids_norm <- ids_norm_new

# Connection to lims 
lims_con <- DBI::dbConnect(odbc::odbc(),
                           Driver = "SQL Server Native Client 11.0",
                           Server = server,
                           Database = database,
                           Trusted_connection = "yes",
                           ApplicationIntent = "ReadOnly",
                           timezone = Sys.timezone(),
                           timezone.out = Sys.timezone()
)
on.exit(DBI::dbDisconnect(lims_con), add = TRUE)


# Create temp table of IDs (best for many IDs)
DBI::dbExecute(lims_con, "IF OBJECT_ID('tempdb..#ids') IS NOT NULL DROP TABLE #ids;")
DBI::dbExecute(lims_con, "CREATE TABLE #ids (id_norm varchar(64) NOT NULL);")
DBI::dbWriteTable(lims_con, "#ids", ids_norm_new, append = TRUE, temporary = TRUE)
DBI::dbExecute(lims_con, "CREATE CLUSTERED INDEX IX_ids ON #ids(id_norm);")


#query 

sql <- glue("
WITH base AS (
  SELECT i.id_norm,
         c.SpecimenDateCollected, c.SpecimenSource,
         c.PatientAddressCountry, c.PatientAddressState, c.PatientAddressCounty, c.PatientGender,c.PatientAge,
         c.SubmitterName,c.SubmitterAddress1, c.SubmitterCity, c.SubmitterState, c.SubmitterZipcode,
         '{lims_common}' AS src_table
  FROM #ids i
  JOIN {`database`}.dbo.{`lims_common`} c
    ON c.PHLAccessionNumber = i.id_norm

  UNION ALL

  SELECT i.id_norm,
         m.SpecimenDateCollected, m.SpecimenSource,
         m.PatientAddressCountry, m.PatientAddressState, m.PatientAddressCounty,m.PatientGender,m.PatientAge,
         m.SubmitterName,m.SubmitterAddress1, m.SubmitterCity, m.SubmitterState, m.SubmitterZipcode,
         '{lims_micro}' AS src_table
  FROM #ids i
  JOIN {`database`}.dbo.{`lims_micro`} m
    ON m.PHLAccessionNumber = i.id_norm
)


SELECT
  b.id_norm AS query_id,
  CAST(b.SpecimenDateCollected AS date) AS collection_date,
  b.SpecimenSource        AS isolation_source,
  b.PatientAddressCountry AS country,
  b.PatientAddressState   AS state,
  b.PatientAddressCounty  AS county,
  b.PatientGender         AS patient_gender,
  b.PatientAge            AS patient_age,
  b.SubmitterName         AS collected_by,
  b.SubmitterAddress1     AS submitter_address,
  b.SubmitterCity         AS submitter_city,
  b.SubmitterState        AS submitter_state,
  b.SubmitterZipcode      AS submitter_zip,
  a.MosquitoSpecies       AS mosquito_species,
  b.src_table
FROM base b
LEFT JOIN {`database`}.dbo.{`lims_arbo`} a
  ON a.PHLAccessionNumber = b.id_norm
")


res <- DBI::dbGetQuery(lims_con, sql)


# Deduplicate here
res <- res %>%
  group_by(query_id) %>%
  summarise(
    collection_date   = max(collection_date, na.rm = TRUE),
    isolation_source  = first(na.omit(isolation_source)),
    country           = first(na.omit(country)),
    state             = first(na.omit(state)),
    county            = first(na.omit(county)),
    patient_gender    = first(na.omit(patient_gender)),  
    patient_age       = first(na.omit(patient_age)),
    collected_by      = first(na.omit(collected_by)),
    submitter_address = first(na.omit(submitter_address)),
    submitter_city    = first(na.omit(submitter_city)),
    submitter_state   = first(na.omit(submitter_state)),
    submitter_zip     = first(na.omit(submitter_zip)),
    mosquito_species  = first(na.omit(mosquito_species)),
    src_table         = paste(unique(src_table), collapse = ";"),
    .groups = "drop"
  )


#concat to single address column for submitter
res <- res %>%
  # clean parts first: trim and treat "" as NA
  mutate(across(
    c(submitter_address, submitter_city, submitter_state, submitter_zip, country),
    ~ na_if(str_trim(.), "")
  )) %>%
  # OPTIONAL light normalization (remove if you don’t want it)
  mutate(
    submitter_state = if_else(is.na(submitter_state), NA, toupper(submitter_state)),
    submitter_zip   = str_replace_all(submitter_zip %||% "", "[^0-9-]", "") %>% na_if("")
  ) %>%
  # create the single field; keep originals with remove = FALSE
  tidyr::unite(
    "submitter_full_address",
    submitter_address, submitter_city, submitter_state, submitter_zip, country,
    sep = ", ", na.rm = TRUE, remove = FALSE
  ) %>%
  mutate(submitter_full_address = na_if(submitter_full_address, ""))

# Join back Description and rename
results <- ids_norm_new %>%
  left_join(res, by = c("id_norm" = "query_id")) %>%
  left_join(samplesheets.df %>% transmute(id_norm = norm_id(wa_id), Description),
            by = "id_norm") %>%
  rename(wa_id = id_norm) %>%
  relocate(wa_id, Description)




print(dplyr::count(results, src_table, sort = TRUE))

# Save results for use in duckDB and metadata scripts. archive samplesheets
samplesheet_dir <- "samplesheets"
archive_dir <- file.path(samplesheet_dir, "archive")

# Create archive folder if it doesn't exist
if (!dir_exists(archive_dir)) {
  dir_create(archive_dir)
}

# Move ALL samplesheet files from samplesheets/ to archive/
samplesheet_files <- dir_ls(samplesheet_dir, glob = "*.csv", type = "file")

for (file in samplesheet_files) {
  archive_path <- path(archive_dir, path_file(file))
  file_move(file, archive_path)
  message("Archived: ", path_file(file))
}

saveRDS(results, file = file.path(secure_path, "lims_query_results.rds"))

#write to a csv for checks
lims_results_dir <- file.path(secure_path, "lims_results")
if (!dir.exists(lims_results_dir)) {
  dir.create(lims_results_dir, recursive = TRUE)
}

# Build timestamped filename in the results folder
outfile <- file.path(
  lims_results_dir,
  sprintf("lims_query_results_full_%s.csv",
          format(Sys.time(), "%Y%m%d_%H%M%S"))
)

utils::write.csv(results, outfile, row.names = FALSE, na = "")


