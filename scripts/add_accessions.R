#### run to add new NCBI accession ID's to the duckdb file run `ACCESSIONS_CSV="file_accessions".csv Rscript scripts/add_accessions.R`


library(DBI)
library(duckdb)
library(readr)
library(dplyr)
library(stringr)

# --- Config ---
clean_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  stringr::str_trim(x)
}

db_path <- Sys.getenv("DUCKDB_PATH_PROD")
csv_path <- Sys.getenv(
  "ACCESSIONS_CSV",
  unset = file.path(getwd(), "accessions.csv")
)
fill_missing_only <- TRUE   # TRUE = fill only NULLs; FALSE = overwrite existing

# --- Connect ---
con <- dbConnect(duckdb(db_path, read_only = FALSE))
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS ncbi_identifiers (
  anon_id   TEXT,
  wa_id     TEXT,
  pathogen  TEXT,
  id_type   TEXT,
  accession TEXT,
  segment   TEXT
);
")

# Read accession file: supports .csv or .tsv/.txt
ext <- tools::file_ext(csv_path)

acc <- if (tolower(ext) %in% c("tsv", "txt")) {
  readr::read_tsv(csv_path, show_col_types = FALSE)
} else if (tolower(ext) == "csv") {
  readr::read_csv(csv_path, show_col_types = FALSE)
} else {
  stop("Unsupported accession file type: ", ext)
}

# Normalize column names + values
acc <- acc |>
  rename_with(tolower)

# Make sure the required columns are present
stopifnot(all(c("id_type", "accession", "sequence_id") %in% names(acc)))

acc <- acc %>%
  mutate(
    id_type     = tolower(clean_text(id_type)),
    accession   = clean_text(accession),
    sequence_id = toupper(clean_text(sequence_id)),
    waphl_base  = str_extract(sequence_id, "WAPHL-\\d+"),
    segment     = str_match(sequence_id, "WAPHL-\\d+-(\\d+)$")[,2]
  ) %>%
  filter(!is.na(waphl_base), !is.na(accession), accession != "") %>%
  filter(id_type %in% c("genbank", "biosample", "sra")) %>%
  distinct()

# Register as temporary DuckDB table
dbExecute(con, "DROP TABLE IF EXISTS staging_acc;")
dbWriteTable(con, "staging_acc", acc, temporary = TRUE, overwrite = TRUE)

rows_inserted <- dbExecute(con, "
INSERT INTO ncbi_identifiers (anon_id, wa_id, pathogen, id_type, accession, segment)
SELECT
  a.anon_id,
  a.wa_id,
  a.pathogen,
  s.id_type,
  s.accession,
  CASE
    WHEN s.id_type = 'genbank' THEN s.segment
    ELSE NULL
  END AS segment
FROM anon_ids a
JOIN staging_acc s
  ON a.anon_id LIKE '%' || s.waphl_base || '%'
WHERE NOT EXISTS (
  SELECT 1
  FROM ncbi_identifiers n
  WHERE n.anon_id   = a.anon_id
    AND n.id_type   = s.id_type
    AND n.accession = s.accession
    AND COALESCE(n.segment,'') = COALESCE(
      CASE WHEN s.id_type='genbank' THEN s.segment ELSE NULL END, ''
    )
);
")




