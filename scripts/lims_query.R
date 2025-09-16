

#load libraries 
library(DBI)
library(odbc)  
library(tidyverse)
library(glue)

readRenviron("../.Renviron")  #load renviron file here

# Configs (including multi tables)
#starLIMS_path <- Sys.getenv("STARLIMS_PATH")
secure_path <- Sys.getenv("SECURE_PATH")
lims_common <- Sys.getenv("TABLE_COMMON")
lims_micro <- Sys.getenv("TABLE_MICRO")
lims_arbo <- Sys.getenv("TABLE_ARBO")
database <- Sys.getenv("DATABASE")
server <- Sys.getenv("SERVER")

#server   <- db_info$server[1]
#database <- db_info$database[1]
#tables   <- unique(db_info$table)

esc<- function(x) gsub("'", "''", x)




#read lines to get the data section only of wonky samplesheet formatting
read_samplesheet_data <- function(file_path) {
  lines <- readLines(file_path)
  data_start <- grep("^\\[Data\\]", lines)
  if (length(data_start) == 0) return(NULL)
  read.csv(text = paste(lines[(data_start + 1):length(lines)], collapse = "\n"), stringsAsFactors = FALSE)
}


samplesheet_dir <- "samplesheets"
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

samplesheets.df <- samplesheets.df %>%
  mutate(
    wa_id = str_extract(Sample_ID, "WA\\d+" ))%>%
  filter(!is.na(wa_id) & wa_id != "") %>%
  distinct(wa_id, Description)

norm_id <- function(x) toupper(trimws(x))
ids_norm <- samplesheets.df %>%
  transmute(id_norm = norm_id(wa_id)) %>%
  distinct()

# Connection
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
DBI::dbWriteTable(lims_con, "#ids", ids_norm, append = TRUE, temporary = TRUE)

#query 

sql <- glue("
WITH base AS (
  SELECT i.id_norm,
         c.SpecimenDateCollected, c.SpecimenSource,
         c.PatientAddressCountry, c.PatientAddressState, c.PatientAddressCounty,
         c.SubmitterName,
         '{lims_common}' AS src_table
  FROM #ids i
  JOIN {`database`}.dbo.{`lims_common`} c
    ON UPPER(RTRIM(LTRIM(CAST(c.PHLAccessionNumber AS varchar(64))))) = i.id_norm

  UNION 

  SELECT i.id_norm,
         m.SpecimenDateCollected, m.SpecimenSource,
         m.PatientAddressCountry, m.PatientAddressState, m.PatientAddressCounty,
         m.SubmitterName,
         '{lims_micro}' AS src_table
  FROM #ids i
  JOIN {`database`}.dbo.{`lims_micro`} m
    ON UPPER(RTRIM(LTRIM(CAST(m.PHLAccessionNumber AS varchar(64))))) = i.id_norm
    
)


SELECT
  b.id_norm AS query_id,
  CAST(b.SpecimenDateCollected AS date) AS collection_date,
  b.SpecimenSource        AS isolation_source,
  b.PatientAddressCountry AS country,
  b.PatientAddressState   AS state,
  b.PatientAddressCounty  AS county,
  b.SubmitterName         AS collected_by,
  a.MosquitoSpecies       AS mosquito_species,   
  b.src_table
FROM base b
LEFT JOIN {`database`}.dbo.{`lims_arbo`} a
  ON UPPER(RTRIM(LTRIM(CAST(a.PHLAccessionNumber AS varchar(64))))) = b.id_norm
ORDER BY query_id;
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
    collected_by      = first(na.omit(collected_by)),
    mosquito_species  = first(na.omit(mosquito_species)),
    src_table         = paste(unique(src_table), collapse = "; "),
    .groups = "drop"
  )

# Join back Description and rename
results <- ids_norm %>%
  left_join(res, by = c("id_norm" = "query_id")) %>%
  left_join(samplesheets.df %>% transmute(id_norm = norm_id(wa_id), Description),
            by = "id_norm") %>%
  rename(wa_id = id_norm) %>%
  relocate(wa_id, Description)




print(dplyr::count(results, src_table, sort = TRUE))

# Save results for use in duckDB and metadata scripts 
saveRDS(results, file = file.path(secure_path, "lims_query_results.rds"))

outfile <- sprintf("lims_query_results_full_%s.csv",
                   format(Sys.time(), "%Y%m%d_%H%M%S"))

utils::write.csv(results, outfile, row.names = FALSE, na = "")


