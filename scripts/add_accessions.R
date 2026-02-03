
library(DBI)
library(duckdb)
library(readr)
library(dplyr)
library(stringr)

# --- Config ---
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

# ---------------------------------------------------------
# ONE-TIME BACKFILL:
# Populate ncbi_identifiers from legacy anon_ids columns
# Safe to re-run (uses NOT EXISTS)
# ---------------------------------------------------------

DBI::dbExecute(con, "
INSERT INTO ncbi_identifiers (anon_id, wa_id, pathogen, id_type, accession, segment)
SELECT a.anon_id, a.wa_id, a.pathogen, 'genbank', a.genbank, NULL
FROM anon_ids a
WHERE a.genbank IS NOT NULL AND a.genbank <> ''
  AND NOT EXISTS (
    SELECT 1 FROM ncbi_identifiers n
    WHERE n.anon_id = a.anon_id AND n.id_type = 'genbank'
  );
")

DBI::dbExecute(con, "
INSERT INTO ncbi_identifiers (anon_id, wa_id, pathogen, id_type, accession, segment)
SELECT a.anon_id, a.wa_id, a.pathogen, 'biosample', a.biosample, NULL
FROM anon_ids a
WHERE a.biosample IS NOT NULL AND a.biosample <> ''
  AND NOT EXISTS (
    SELECT 1 FROM ncbi_identifiers n
    WHERE n.anon_id = a.anon_id AND n.id_type = 'biosample'
  );
")



# Read the accession CSV
acc <- read_csv(csv_path, show_col_types = FALSE)

# Normalize column names + values
acc <- acc |>
  rename_with(tolower)

# Make sure the required columns are present
stopifnot(all(c("id_type", "accession", "sequence_id") %in% names(acc)))

acc <- acc %>%
  mutate(
    id_type    = tolower(str_trim(as.character(id_type))),
    accession  = str_trim(as.character(accession)),
    sequence_id = toupper(str_trim(as.character(sequence_id))),
    waphl_base = str_extract(sequence_id, "WAPHL-\\d+"),
    segment    = str_match(sequence_id, "WAPHL-\\d+-(\\d+)$")[,2]
  ) %>%
  filter(!is.na(waphl_base), !is.na(accession), accession != "") %>%
  filter(id_type %in% c("genbank", "biosample", "sra")) %>%
  distinct()

# Register as temporary DuckDB table
dbExecute(con, "DROP TABLE IF EXISTS staging_acc;")
dbWriteTable(con, "staging_acc", acc, temporary = TRUE, overwrite = TRUE)

dbGetQuery(con, "
SELECT
  a.wa_id,
  a.anon_id,
  s.sequence_id,
  s.accession,
  s.id_type,
  s.segment
FROM anon_ids a
JOIN staging_acc s
  ON a.anon_id LIKE '%' || s.waphl_base || '%'
LIMIT 20;
")


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

# --- Debug: show unmatched staging rows (no anon_ids match) ---
unmapped <- dbGetQuery(con, "
SELECT s.sequence_id, s.waphl_base, s.id_type, s.accession
FROM staging_acc s
LEFT JOIN anon_ids a
  ON a.anon_id LIKE '%' || s.waphl_base || '%'
WHERE a.anon_id IS NULL
LIMIT 50;
")
if (nrow(unmapped) > 0) {
  message("WARNING: These rows did not match any anon_id (showing up to 50):")
  print(unmapped)
}

# --- Preview inserted identifiers ---
print(dbGetQuery(con, "
SELECT wa_id, anon_id, id_type, accession, segment
FROM ncbi_identifiers
ORDER BY anon_id, id_type, segment, accession
LIMIT 25;
"))



