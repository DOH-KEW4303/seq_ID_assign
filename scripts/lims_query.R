### You will need a .Renviron file containing starlims path, database info, server info etc. Script will extract internal ID from the Basespace samplesheet (along with the pathogen descriptor), confirm new samples (not already in DuckDB file), query LIMS for 
### the required metadata. Query results are saved and will be exported to a csv file with a later script

#load libraries 
library(DBI)
library(odbc)  
library(tidyverse)
library(glue)
library(fs)
library(duckdb)

#readRenviron("../.Renviron")  #load renviron file here

# Configs (including multi tables)
starLIMS_path <- Sys.getenv("STARLIMS_PATH")
secure_path <- Sys.getenv("SECURE_PATH")
lims_common <- Sys.getenv("TABLE_COMMON")
lims_micro <- Sys.getenv("TABLE_MICRO")
lims_arbo <- Sys.getenv("TABLE_ARBO")
lims_flu <-Sys.getenv("TABLE_FLU")
database <- Sys.getenv("DATABASE")
server <- Sys.getenv("SERVER")


esc<- function(x) gsub("'", "''", x)


#read in csv summary files from binfx pipelines in the binfx_results folder

# helper function to handle col names across files
standardize_summary <- function(df) {
  
  if (all(c("WGS_ID", "Final_Taxa_ID") %in% names(df))) {
    
    out <- df %>%
      transmute(
        wa_id = str_extract(WGS_ID, "WA\\d+"),
        pathogen = Final_Taxa_ID
      )
    
  } else if (
    any(grepl("^entity:.*_id$", names(df))) &&
    "gambit_predicted_taxon" %in% names(df)
  ) {
    entity_id_col <- names(df)[
      grepl("^entity:.*_id$", names(df))
    ][1]
    
    out <- df %>%
      transmute(
        wa_id = str_extract(.data[[entity_id_col]], "WA\\d+"),
        pathogen = gambit_predicted_taxon
      )
    
  } else if (all(c("id", "species") %in% names(df))) {
    
    out <- df %>%
      transmute(
        wa_id = str_extract(id, "WA\\d+"),
        pathogen = species
      )
    
  } else {
    stop(
      "Unrecognized input file format. Columns found: ",
      paste(names(df), collapse = ", ")
    )
  }
  
  out %>%
    filter(!is.na(wa_id) & wa_id != "") %>%
    distinct(wa_id, pathogen)
}

summary_dir <- "binfx_results"

summary_files <- list.files(
  path = summary_dir,
  pattern = "\\.csv|tsv$",
  full.names = TRUE,
  ignore.case = TRUE
)

summary_list <- lapply(summary_files, function(file) {
  
  if (grepl("\\.tsv$", file, ignore.case = TRUE)) {
    df <- read_tsv(file, show_col_types = FALSE)
  } else {
    df <- read_csv(file, show_col_types = FALSE)
  }
  
  standardize_summary(df)
})


sample_pathogen.df <- bind_rows(summary_list) %>%
  distinct(wa_id, pathogen)


norm_id <- function(x) toupper(trimws(x))

ids_norm <- sample_pathogen.df %>%
  transmute(id_norm = norm_id(wa_id)) %>%
  distinct()

print(sample_pathogen.df)

# Connect to DuckDB and fetch existing wa_ids
run_env <- Sys.getenv("RUN_ENV", unset = "DEV")
db_path <- if (run_env == "PROD") {
  Sys.getenv("DUCKDB_PATH_PROD")
} else {
  Sys.getenv("DUCKDB_PATH_DEV")
}

message("RUN_ENV: ", run_env)
message("DuckDB path: ", db_path)

con_duck <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = db_path,
  read_only = TRUE
)

message("DuckDB valid: ", DBI::dbIsValid(con_duck))

existing_wa <- DBI::dbGetQuery(
  con_duck,
  "SELECT wa_id FROM anon_ids"
) %>%
  mutate(wa_id = norm_id(wa_id)) %>%
  distinct()

DBI::dbDisconnect(con_duck)
                  
# Keep only WA IDs not already in duckdb
ids_norm_new <- ids_norm %>%
  anti_join(existing_wa, by = c("id_norm" = "wa_id"))

message("Total WA IDs in binfx results: ", nrow(ids_norm))
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
#on.exit(DBI::dbDisconnect(lims_con), add = TRUE)


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
derive_flu_subtype <- function(result_text) {
    if (is.null(result_text)) return(NA_character_)
  
    dplyr::case_when(
      is.na(result_text) ~ NA_character_,
    
      stringr::str_detect(
        result_text,
        "A\\(H1N1\\)pdm09|2009\\s*H1N1"
      ) ~ "A(H1N1)pdm09",
    
      stringr::str_detect(
        result_text,
        "A\\(H3\\)|\\(H3\\)"
      ) ~ "A(H3)",
    
      stringr::str_detect(
        result_text,
        "A\\(H5\\)|\\(H5\\)"
      ) ~ "A(H5)",
    
      stringr::str_detect(
        result_text,
        "B/Victoria"
      ) ~ "B/Victoria",
    
      stringr::str_detect(
        result_text,
        "B/Yamagata"
      ) ~ "B/Yamagata",
    
      stringr::str_detect(
        result_text,
        "Influenza B virus detected"
      ) ~ "B",
    
      stringr::str_detect(
        result_text,
        "Subtype undetected"
      ) ~ "A (unsubtyped)",
    
      TRUE ~ NA_character_
    )
}

# Identify only influenza samples
flu_ids <- sample_pathogen.df %>%
  filter(
    str_detect(
      pathogen,
      fixed("Alphainfluenzavirus influenzae", ignore_case = TRUE)
    ) |
      str_detect(
        pathogen,
        fixed("Betainfluenzavirus influenzae", ignore_case = TRUE)
      )
  ) %>%
  transmute(id_norm = norm_id(wa_id)) %>%
  distinct()


# Query influenza table only if this run actually has flu samples
if (nrow(flu_ids) > 0) {
  
  DBI::dbExecute(
    lims_con,
    "IF OBJECT_ID('tempdb..#flu_ids') IS NOT NULL DROP TABLE #flu_ids;"
  )
  
  DBI::dbExecute(
    lims_con,
    "CREATE TABLE #flu_ids (id_norm varchar(64) NOT NULL);"
  )
  
  DBI::dbWriteTable(
    lims_con,
    "#flu_ids",
    flu_ids,
    append = TRUE,
    temporary = TRUE
  )
  
  flu_sql <- glue("
    SELECT id_norm, ResultTextConclusion
    FROM (
      SELECT
        f.PHLAccessionNumber AS id_norm,
        f.ResultTextConclusion,
        ROW_NUMBER() OVER (
          PARTITION BY f.PHLAccessionNumber
          ORDER BY f.SpecimenDateCollected DESC
        ) AS rn
      FROM {`database`}.dbo.{`lims_flu`} f
      JOIN #flu_ids i
        ON f.PHLAccessionNumber = i.id_norm
      WHERE
        f.ResultTextConclusion LIKE 'Influenza%'
    ) ranked
    WHERE rn = 1
  ")
  
  flu_res <- DBI::dbGetQuery(lims_con, flu_sql)
  
  res <- res %>%
    left_join(
      flu_res,
      by = c("query_id" = "id_norm")
    ) %>%
    rename(
      influenza_result_text = ResultTextConclusion
    )
  
} else {
  
  # Important: column still exists even on non-flu runs
  res$influenza_result_text <- NA_character_
}

res <- res %>%
  mutate(
    flu_subtype = derive_flu_subtype(influenza_result_text)
  )

#concat to single address column for submitter
res <- res %>%
  # clean parts first: trim and treat "" as NA
  mutate(across(
    c(submitter_address, submitter_city, submitter_state, submitter_zip, country),
    ~ na_if(str_trim(.), "")
  )) %>%
  # normalize 
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

# Join back pathogen and rename 
results <- ids_norm_new %>%
  left_join(res, by = c("id_norm" = "query_id")) %>%
  left_join(sample_pathogen.df %>% transmute(id_norm = norm_id(wa_id), pathogen),
            by = "id_norm") %>%
  rename(wa_id = id_norm) %>%
  relocate(wa_id, pathogen)




print(dplyr::count(results, src_table, sort = TRUE))

# Save results for use in duckDB and metadata scripts. archive samplesheets
archive_dir <- file.path(summary_dir, "archive")

# Create archive folder if it doesn't exist
if (!dir_exists(archive_dir)) {
  dir_create(archive_dir)
}

# Move ALL csv files from samplesheets/ to archive/
summary_files <- dir_ls(summary_dir, glob = "*.csv", type = "file")
if (!dir_exists(archive_dir)) {
  dir_create(archive_dir)
}
summary_files_to_archive <- dir_ls(
  summary_dir,
  regexp = "\\.(csv|tsv)$",
  type = "file"
)

for (file in summary_files_to_archive) {
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


