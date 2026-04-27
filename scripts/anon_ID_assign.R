#################################
#Reads in rds file with lims query results, connects to duckdb and executes function to assign random identifier to wach WA record. writes to duckdb file on the y drive. 
library(DBI)
library(duckdb)
library(dplyr)
library(lubridate)
library(arrow)
library(fs)
library(stringr)

# load paths from Renviron and read .rds file 
secure_path <- Sys.getenv("SECURE_PATH")
results <- readRDS(file = file.path(secure_path, "lims_query_results.rds"))

# Determine environment and load appropriate paths. defaults to DEV if not specified 
run_env <- Sys.getenv("RUN_ENV", unset = "DEV")

if (run_env == "PROD") {
  db_path <- Sys.getenv("DUCKDB_PATH_PROD")
  lock_path <- Sys.getenv("LOCK_PATH_PROD")
} else {
  db_path <- Sys.getenv("DUCKDB_PATH_DEV")
  lock_path <- Sys.getenv("LOCK_PATH_DEV")
}


#define function to create timestamped snapshots to parquet file
snapshot_dir <- file.path(secure_path, "snapshots")
snapshot_to_parquet <- function(df, base_name = "anon_id_snapshot", output_dir = snapshot_dir) {
  dir_create(output_dir)  # Make sure the snapshots folder exists
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_name <- paste0(base_name, "_", timestamp, ".parquet")
  file_path <- path(output_dir, file_name)
  
  write_parquet(df, file_path)
  message("Snapshot written to: ", file_path)
  return(file_path)
}

#define function to log warning and error messages
if (!dir.exists("logs")) dir.create("logs")
log_file <- file.path("logs", paste0("anon_id_assignment_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".log"))

log_message <- function(message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_msg <- paste0("[", timestamp, "] ", message, "\n")
  cat(full_msg, file = log_file, append = TRUE)
}


#define function-check for exisiting lock file and display info if present
check_and_create_lock <-function(lock_path) {
  if(file.exists(lock_path)) {
  lock_info <- readLines(lock_path, warn = FALSE)
  stop(paste0(
    "The database file is currently locked / in use.\n",
    "Locked by:" , lock_info[1], "\n",
    "since: ", lock_info[2], "\n",
    "Try again later."))
  }
  
  #create the user and timestamp info for lock
  lock_info <- c(Sys.info()[["user"]], as.character(Sys.time()))
  writeLines(lock_info, lock_path)
}

# Create/connect to the DuckDB file and create table and write function to assign ID's 
assign_anon_ids <- function(results, db_path, lock_path) {
  check_and_create_lock(lock_path)
  
  #remove lock file after process is finished
  on.exit({
    if (file.exists(lock_path)) {
      file.remove(lock_path)
    }
  }, add = TRUE)
  
  # Connect to duckdb (read-only = FALSE because we're inserting)
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  
  
  
  # Create table if it doesn't exist yet (ours will already exist)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS anon_ids (
      wa_id TEXT PRIMARY KEY,
      anon_id TEXT,
      collection_date INTEGER,
      pathogen TEXT,
      biosample TEXT,
      genbank TEXT
    )
  ")
  
  # If the table already exists (older schema), make sure new columns are added
  dbExecute(con, "ALTER TABLE anon_ids ADD COLUMN IF NOT EXISTS biosample TEXT;")
  dbExecute(con, "ALTER TABLE anon_ids ADD COLUMN IF NOT EXISTS genbank   TEXT;")
  
  
  # ---- NEW: table to store multiple NCBI identifiers per sample (segments, SRRs, etc.) ----
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS ncbi_identifiers (
    anon_id    TEXT,
    wa_id      TEXT,
    pathogen   TEXT,
    id_type    TEXT,        
    accession  TEXT,
    segment    TEXT
  )
")
  
  # Schema guard (safe if table existed previously with fewer columns)
  dbExecute(con, "ALTER TABLE ncbi_identifiers ADD COLUMN IF NOT EXISTS anon_id    TEXT;")
  dbExecute(con, "ALTER TABLE ncbi_identifiers ADD COLUMN IF NOT EXISTS wa_id      TEXT;")
  dbExecute(con, "ALTER TABLE ncbi_identifiers ADD COLUMN IF NOT EXISTS pathogen   TEXT;")
  dbExecute(con, "ALTER TABLE ncbi_identifiers ADD COLUMN IF NOT EXISTS id_type    TEXT;")
  dbExecute(con, "ALTER TABLE ncbi_identifiers ADD COLUMN IF NOT EXISTS accession  TEXT;")
  dbExecute(con, "ALTER TABLE ncbi_identifiers ADD COLUMN IF NOT EXISTS segment    TEXT;")
  
  
  # Define descriptor-to-prefix mapping 
  descriptor_prefixes <- list(
    "InfluenzaA" = "A",
    "InfA_H1" = "A",
    "InfA_H3" ="A",
    "InfA_H1_H3" = "A",
    "IfnA_H1pdm" = "A",
    "IfnA_H3" = "A",
    "InfA" = "A",
    "IfnB" = "B",
    "InfB" = "B",
    "SARS-CoV-2" = "SARS-CoV-2",
    "Corynebacterium_diphtheriae" = "cDiph",
    "Corynebacterium_ulcerans" = "cUlcerans",
    "Measles" = "MVs",
    "Mumps_virus" = "MuV",
    "Adenovirus" = "HAdV",
    "HIV" = "hIV",
    "Hepatitis B" = "HepBV",
    "Zika" = "ZikaV",
    "WNV" = "WNV",
    "Norovirus" = "Norovirus",
    "Dengue" = "DenV",
    "Mpox" = "MpoxV",
    "RSV-A" = "RSvA",
    "RSV-B" = "RSVB",
    "HSV" = "HSV",
    "Mycobacterium_tuberculosis" = "Mtb",
    "Staphylococcus_aureus" = "staphA",
    "Salmonella_enterica" = "PulseNet",
    "Shigella" = "PulseNet",
    "Escherichia_coli" = "PulseNet",
    "Camplyocbacter_jejuni" = "PulseNet",
    "Listeria_monocytogenes" = "PulseNet",
    "Vibrio" = "PulseNet",
    "Vibrio_cholerae" ="PulseNet"
  )
  
  
  
  # check first for WA ID if record already exists in db. if not assign the new anon ID to the new WA ID. 
  results <- results %>%
    rename(pathogen = Description) %>%
    mutate(
      descriptor_norm = sub("_.*$", "", pathogen),
      prefix = descriptor_prefixes[descriptor_norm]
    ) %>%
    rowwise() %>%
    mutate(
      anon_id = {
        if (!is.na(isolation_source) && 
            grepl("^Raw\\s*Wastewater\\s*(Composite|Grab)$",
                  isolation_source, ignore.case = TRUE)) {
          msg <- paste("Skipping WA ID due to excluded isolation_source:",
                       wa_id, "-", isolation_source)
          warning(msg)
          log_message(msg)
          NA_character_
          
        } else if (any(c(
          is.na(wa_id), wa_id == "",
          is.na(collection_date), collection_date == "",
          is.na(pathogen), pathogen == ""
        ), na.rm = TRUE)) {
          msg <- paste("Skipping row due to missing required value for:", wa_id)
          warning(msg)
          log_message(msg)
          NA_character_
          
        } else {
          existing_id <- dbGetQuery(con, paste0(
          "SELECT anon_id FROM anon_ids WHERE wa_id = '", wa_id, "'"
        ))
        
          if (nrow(existing_id) > 0) {
           msg <- paste("WA ID already exists in database:", wa_id, "→ using existing anon_id:", existing_id$anon_id)
           warning(msg)
           log_message(msg)
           existing_id$anon_id
           
          } else {
            coll_date <- as.Date(collection_date)
            year_val  <- year(coll_date)
            prefix    <- descriptor_prefixes[[pathogen]]
            
            if (is.null(prefix)) {
              msg <- paste(
                "Unknown pathogen descriptor for WA ID:",
                wa_id, "-", pathogen, "→ skipping row."
              )
              warning(msg)
              log_message(msg)
              NA_character_
              
            } else {
              new_anon_id <- NA_character_
          
              repeat {
                rand_num <- sprintf("%06d", sample(1e6, 1)) #six digit random number 
                
                if (prefix == "MVs") {
                  # Special measles format:
                  # MVs/Washington.USA/week.year/WAPHL-012345
                  wk       <- isoweek(coll_date)
                  epi_year <- year_val    # use isoyear(coll_date) instead if you want epi-year
                  date_tok <- sprintf("%02d.%d", wk, epi_year)
                  
                  new_anon_id <- paste0(
                    "MVs/Washington.USA/",
                    date_tok,
                    "/WAPHL-",
                    rand_num
                  )
                } else {
                  
                  no_host_prefixes <- c("WNV", "DenV", "ZikaV", "A", "B")
                  
                  if (prefix %in% c("A", "B")) {
                    new_anon_id <- paste0(
                      prefix, "/WASHINGTON/WAPHL-", rand_num, "/", year_val
                    )
                  
                  } else if (prefix %in% no_host_prefixes) {
                    new_anon_id <- paste0(
                      prefix, "/USA/WAPHL-", rand_num, "/", year_val
                      )
                    
                  } else {
                    new_anon_id <- paste0(
                      prefix, "/Human/USA/WAPHL-", rand_num, "/", year_val
                      )
                    }
                  }

            
            # ensure that the new anon ID doesn't accidentally match another ID already in 
               existing <- dbGetQuery(con, paste0(
                 "SELECT 1 FROM anon_ids WHERE anon_id = '", new_anon_id, "'"
                 ))
            
               if (nrow(existing) == 0) {
                 dbExecute(con,
                           "INSERT INTO anon_ids (wa_id, anon_id, collection_date, pathogen)
                           VALUES (?, ?, ?, ?)",
                           params = list(wa_id, new_anon_id, year_val, pathogen)
                )
                break
               }
              } # end repeat
              
              new_anon_id
            } # end else (prefix not NULL)
          } # end else (no existing_id)
        } # end outer else
      } # end anon_id expression
    ) %>%
    ungroup()
  
  dbExecute(con, "CHECKPOINT")
  print(dbListFields(con, "anon_ids"))
  
  dbDisconnect(con)
  return(results)
}
              
          
  




#function to clean the metadata,create a df that can be exported to use for NCBI upload 

export_metadata <- function(results) {
  excluded_sources <- c("Raw Wastewater Composite", "Raw Wastewater Grab")
  
  bioproject_map <- c(
    flu_a   = "PRJNA1400571",
    flu_b   = "PRJNA1400040",
    measles = "PRJNA1365947",
    cov2    = "PRJNA749781",
    wnv     = "PRJNA1320692"
  )
  
  results_filtered <- results %>% 
    filter(!is.na(anon_id) & anon_id != "") %>%
    filter(!grepl("PulseNet", anon_id, fixed = TRUE))%>%
    filter(!grepl("Mtb", anon_id, fixed = TRUE))%>%
    filter(!( !is.na(isolation_source) & isolation_source %in% excluded_sources ))
  
  final <- results_filtered %>%
    select(-wa_id) %>%
    rename( `bs-strain` = anon_id) %>%
    mutate(
      ncbi_bioproject = dplyr::case_when(
        descriptor_norm %in% c("InfluenzaA", "InfA", "IfnA") ~ bioproject_map[["flu_a"]],
        descriptor_norm %in% c("InfluenzaB", "InfB", "IfnB") ~ bioproject_map[["flu_b"]],
        descriptor_norm %in% c("Measles", "MeV") ~ bioproject_map[["measles"]],
        descriptor_norm %in% c("SARS-CoV-2") ~ bioproject_map[["cov2"]],
        descriptor_norm %in% c("WNV") ~ bioproject_map[["wnv"]],
        TRUE ~ NA_character_
      ),
      authors = "Dykema,P.; Hanson,N.;Yang,Q.;Lucas,D.;Grimmet Jr,S.;Johnson,J.;Waterman,K.",
      organism = dplyr::case_when(
        # Influenza A
        stringr::str_detect(stringr::str_to_lower(pathogen), "infa|influenzaa|influenza a") ~ 
          "influenza a virus",
        
        # Influenza B
        stringr::str_detect(stringr::str_to_lower(pathogen), "infb|influenza b") ~ 
          "influenza b virus",
        
        # Measles
        stringr::str_detect(stringr::str_to_lower(pathogen), "measles|mev") ~ 
          "Measles morbillivirus",
        
        TRUE ~ NA_character_
      ),
      `src-Serotype` = dplyr::if_else(
        stringr::str_detect(stringr::str_to_lower(pathogen), "influenza"),
        flu_subtype,
        NA_character_
      ),
      `bs-subtype` = dplyr::if_else(
        stringr::str_detect(stringr::str_to_lower(pathogen), "influenza"),
        flu_subtype,
        NA_character_
      ),
      collection_date = as.character(format(as.Date(collection_date), "%Y-%m-%d")),
      collected_by = collected_by,
      `gb-sample_name` = str_extract(`bs-strain`, "WAPHL-\\d{6,7}"),
      `src-geo_loc_name` = if_else(
        is.na(county),
        "USA:Washington",
        paste0("USA:Washington,", county)
      ),
      `src-Host` = "Homo sapiens",
      `src-Strain` = `bs-strain`,
      `bs-isolate` = `gb-sample_name`,
      `src-Isolate` = `gb-sample_name`,
      `src-Isolation_source` = isolation_source,
      `bs-sample_title` = "",
      `bs-collected_by` = collected_by,
      `bs-geo_loc_name` = `src-geo_loc_name`,
      `bs-host` = "Homo sapiens",
      sequence_name = "",
      `src-geo_loc_name` = if_else(
        is.na(county),
        "USA:Washington",
        paste0("USA:Washington,", str_to_title(county))
      ),
      `src-Host` =  case_when(
        !is.na(mosquito_species) & mosquito_species != "" ~ mosquito_species,
        TRUE ~ "Homo sapiens"
      ),
      `src-Isolate` = `bs-isolate`,
      `src-Isolation_source` = isolation_source,
      `bs-sample_title` = "",
      `bs-collected_by` = collected_by,
      `bs-geo_loc_name` = "USA:Washington",
      `bs-lat_lon` = "missing",
      `bs-host` = `src-Host`,
      `bs-host_disease` = "",
      `bs-isolation_source` = isolation_source,
      `bs-purpose_of_sampling` = "passive surveillance",
      `bs-purpose_of_sequencing` = "sentinel_serveillance",
      `bs-sample_name` = `gb-sample_name`,
      illumina_sequence_instrument = "",
      illumina_library_source = "",
      illumina_library_strategy = "",
      illumina_library_layout = "",
      illumina_library_protocol = "",
      illumina_sra_file_path1 = "",
      illumina_sra_file_path2 = ""
    ) %>%

    dplyr::select(-county, -state, -country, -src_table, -mosquito_species, -descriptor_norm)
  
  # convert any list-columns to semicolon-separated strings 
  final <- final %>%
    mutate(across(where(is.list), ~ vapply(., function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      paste(as.character(x), collapse = ";")
    }, character(1))))
  
  #export schema 
  keep_cols <- c(
    "collection_date", "flu_subtype", "bs-strain", "ncbi_bioproject", "authors", "organism",
    "src-Serotype", "bs-subtype", "gb-sample_name", "src-geo_loc_name", "src-Host",
    "src-Strain", "bs-isolate", "src-Isolate", "src-Isolation_source",
    "bs-sample_title", "bs-collected_by", "bs-geo_loc_name", "bs-host", "sequence_name",
    "bs-lat_lon", "bs-host_disease", "bs-isolation_source", "bs-purpose_of_sampling",
    "bs-purpose_of_sequencing", "bs-sample_name", "illumina_sequence_instrument",
    "illumina_library_source", "illumina_library_strategy", "illumina_library_layout",
    "illumina_library_protocol", "illumina_sra_file_path1", "illumina_sra_file_path2"
  )
  
  missing_cols <- setdiff(keep_cols, names(final))
  for (col in missing_cols) final[[col]] <- NA_character_
  final <- final[, keep_cols]
           
  csv_filename <- file.path(
    Sys.getenv("EXPORT_PATH"),
    sprintf("anon_metadata_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  
  
  
  
  write.csv(final, csv_filename, row.names = FALSE)
  cat("Exported to:", csv_filename, "\n")
  
  return(final)

}


#execute functions
results <- assign_anon_ids(results, db_path, lock_path)
snapshot_to_parquet(results, output_dir = snapshot_dir)
export_metadata(results)
#delete rds file?