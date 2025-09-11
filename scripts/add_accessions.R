
library(DBI)
library(duckdb)
library(readr)
library(dplyr)
library(stringr)

# --- Config ---
db_path <- Sys.getenv("DUCKDB_PATH_PROD")

csv_path <- file.path(getwd(), "VSP_accessions091025.csv")
fill_missing_only <- TRUE   # TRUE = fill only NULLs; FALSE = overwrite existing

# --- Connect ---
con <- dbConnect(duckdb(db_path, read_only = FALSE))
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
# Connect to DB
con <- dbConnect(duckdb(db_path, read_only = FALSE))
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

# Read the accession CSV
acc <- read_csv(csv_path, show_col_types = FALSE)

# Normalize column names + values
acc <- acc |>
  rename_with(tolower) |>
  rename(
    waphl     = any_of(c("waphl", "anon_id_short", "short_id")),
    biosample = any_of("biosample"),
    genbank   = any_of("genbank")
  ) |>
  mutate(
    waphl = toupper(str_trim(waphl))
  )

# Make sure the required columns are present
stopifnot(all(c("waphl", "biosample", "genbank") %in% names(acc)))

# Register as temporary DuckDB table
dbExecute(con, "DROP TABLE IF EXISTS staging_acc;")
dbWriteTable(con, "staging_acc", acc, temporary = TRUE, overwrite = TRUE)

# Do the update: match short ID (WAPHL-######) from anon_id to waphl
rows_updated <- dbExecute(con, "
  UPDATE anon_ids AS a
  SET
    biosample = s.biosample,
    genbank   = s.genbank
  FROM staging_acc s
  WHERE
    REPLACE(
      REGEXP_REPLACE(
        REGEXP_EXTRACT(a.anon_id, '(WA[- ]?PHL[- ]?\\d{6})', 1),
        'WA[- ]?PHL', 'WAPHL'
      ),
      ' ', ''
    ) = s.waphl;
")

  
dbGetQuery(con, "
  SELECT wa_id, anon_id, biosample, genbank
  FROM anon_ids
  WHERE biosample IS NOT NULL OR genbank IS NOT NULL
  ORDER BY wa_id
  LIMIT 10;
")



