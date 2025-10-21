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
      genbank TEXT,
    )
  ")
  
  # If the table already exists (older schema), make sure new columns are added
  dbExecute(con, "ALTER TABLE anon_ids ADD COLUMN IF NOT EXISTS biosample TEXT;")
  dbExecute(con, "ALTER TABLE anon_ids ADD COLUMN IF NOT EXISTS genbank   TEXT;")
  
  # Define descriptor-to-prefix mapping 
  descriptor_prefixes <- list(
    "Influenza A" = "FluA",
    "Influenza B" = "FluB",
    "SARS-CoV-2" = "SARS-CoV-2",
    "Corynebacterium_diphtheriae" = "cDiph",
    "Corynebacterium_ulcerans" = "cUlcerans",
    "Measles_virus" = "hMV",
    "Mumps_virus" = "MuV",
    "Adenovirus" = "HAdV",
    "HIV" = "hIV",
    "Hepatitis B" = "HepBV",
    "Zika" = "ZikaV",
    "WNV" = "WNV",
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
           
        }  else {
          # extract year
          year <- year(as.Date(collection_date))
          new_anon_id <- NA_character_
          
          #try until we get a unique anon ID
          repeat {
            rand_num <- sprintf("%06d", sample(1e6, 1)) #generate padded 6 digit no 
            prefix <- descriptor_prefixes[[pathogen]]
            
            if (is.null(prefix)) {
              msg <- paste("Unknown pathogen descriptor for WA ID:", wa_id, "-", pathogen, "→ skipping row.")
              warning(msg)
              log_message(msg)
              new_anon_id <- NA_character_
              break
            }
            no_host_prefixes <- c("WNV", "DenV", "ZikaV")
            if (prefix %in% no_host_prefixes) {
              new_anon_id <- paste0(prefix, "/USA/WAPHL-", rand_num, "/", year)
            } else {
              new_anon_id <- paste0(prefix, "/Human/USA/WAPHL-", rand_num, "/", year)
            }

            
            # ensure that the new anon ID doesn't accidentally match another ID already in 
            existing <- dbGetQuery(con, paste0(
              "SELECT 1 FROM anon_ids WHERE anon_id = '", new_anon_id, "'"
            ))
            
            if (nrow(existing) == 0) {
              # Insert new identifier into DuckDB
              dbExecute(con, "INSERT INTO anon_ids (wa_id, anon_id, collection_date, pathogen) VALUES (?, ?, ?, ?)",
                        params = list(wa_id, new_anon_id, year, pathogen)
              )
              break
            }
          }
          
          new_anon_id
          }
        }
      }
    ) %>%
    ungroup()
  dbExecute(con, "CHECKPOINT")
  print(dbListFields(con, "anon_ids"))
  
  dbDisconnect(con)
  return(results)
}


#function to drop the WA ID, PN organisms, Mtb, to create a df that can be exported to use for NCBI upload 
export_metadata <- function(results) {
  excluded_sources <- c("Raw Wastewater Composite", "Raw Wastewater Grab")
  
  results_filtered <- results %>% 
    filter(!is.na(anon_id) & anon_id != "") %>%
    filter(!grepl("PulseNet", anon_id, fixed = TRUE))%>%
    filter(!grepl("Mtb", anon_id, fixed = TRUE))%>%
    filter(!( !is.na(isolation_source) & isolation_source %in% excluded_sources ))
  
  final <- results_filtered %>%
    select(-wa_id) %>%
    rename( `bs-Isolate` = anon_id) %>%
    mutate(
      ncbi_bioproject = "",
      authors = "",
      organism = pathogen,
      collection_date = as.character(format(as.Date(collection_date), "%Y-%m-%d")),
      collected_by = collected_by,
      `gb-sample_name` = str_extract(`bs-Isolate`, "WAPHL-\\d{6,7}"),
      `src-geo_loc_name` = if_else(
        is.na(county),
        "USA:Washington",
        paste0("USA:Washington,", county)
      ),
      `src-Host` = "Homo sapiens",
      `src-Isolate` = `bs-Isolate`,
      `src-Isolation_source` = isolation_source,
      `bs-sample_title` = "",
      `bs-collected_by` = collected_by,
      `bs-geo_loc_name` = `src-geo_loc_name`,
      `bs-host` = "Homo sapiens",
      sequence_name = "",
      `gb-sample_name` = str_extract(`bs-Isolate`, "WAPHL-\\d+"),
      `src-geo_loc_name` = if_else(
        is.na(county),
        "USA:Washington",
        paste0("USA:Washington,", str_to_title(county))
      ),
      `src-Host` =  case_when(
        !is.na(mosquito_species) & mosquito_species != "" ~ mosquito_species,
        TRUE ~ "Homo sapiens"
      ),
      `src-Isolate` = `bs-Isolate`,
      `src-Isolation_source` = isolation_source,
      `bs-sample_title` = "",
      `bs-collected_by` = collected_by,
      `bs-geo_loc_name` = "USA:Washington",
      `bs-lat_lon` = "missing",
      `bs-host` = `src-Host`,
      `bs-host_disease` = "",
      `bs-isolation_source` = isolation_source,
      `bs-sample_name` = str_extract(`bs-Isolate`, "WAPHL-\\d+"),
      `gs-sample_name` = "",
      `gs-covv_type` = "betacoronavirus",
      `gs-covv_passage` = "Original",
      `gs-covv_location` = `src-geo_loc_name`,
      `gs-covv_host` = "Homo sapiens",
      `gs-covv_sex` = patient_gender,
      `gs-covv_patient_age` = patient_age,
      `gs-covv_patient_status` = "Unknown",
      `gs-covv_seq_technology` = "Illumina MiSeq",
      `gs-covv_orig_lab` = collected_by,
      `gs-covv_orig_lab_address` = submitter_full_address,
      `gs-covv_comments` = "",
      `gs-comment_type` = "",
      illumina_sequence_instrument = "",
      illumina_library_source = "",
      illumina_library_strategy = "",
      illumina_library_layout = "",
      illumina_library_protocol = "",
      illumina_sra_file_path1 = "",
      illumina_sra_file_path2 = ""
    ) %>%

    select(-county,-state,-country,-src_table, -mosquito_species)
           
           
  csv_filename <- file.path(Sys.getenv("EXPORT_PATH"),
                          paste0("anon_metadata_", Sys.Date(), ".csv"))
  write.csv(final, csv_filename, row.names = FALSE)
  cat("Exported to:", csv_filename, "\n")
  
  return(final)

}


#execute functions
results <- assign_anon_ids(results, db_path, lock_path)
snapshot_to_parquet(results, output_dir = snapshot_dir)
export_metadata(results)
#delete rds file?