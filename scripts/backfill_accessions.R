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