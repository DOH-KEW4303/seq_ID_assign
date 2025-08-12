#################################
#Reads in rds file with lims query results, connects to duckdb and executes function to assign random identifier to wach WA record. writes to duckdb file on the y drive. 
library(DBI)
library(duckdb)
library(dplyr)
library(lubridate)
library(arrow)
library(fs)

# read in rds file with lims query results, Get path from .Renviron for duckdb file and lock file 
secure_path <- Sys.getenv("SECURE_PATH")
db_path <- Sys.getenv("DUCKDB_PATH")
lock_path <- Sys.getenv("LOCK_PATH")
results <- readRDS(file = file.path(secure_path, "lims_query_results.rds"))


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
      Description TEXT
    )
  ")
  # Define descriptor-to-prefix mapping 
  descriptor_prefixes <- list(
    "Influenza A" = "FluA",
    "Influenza B" = "FluB",
    "SARS-CoV-2" = "SARS-Cov-2",
    "Corynebacterium_diphtheriae" = "Cdiph",
    "Measles" = "hMV",
    "Mumps" = "MuV",
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
    "Staphylococcus_aureus" = "staphA",
    "wastewater" = "WW/Metagenomic",
    "Salmonella_enterica" = "PulseNet",
    "Shigella" = "PulseNet",
    "Escherichia_coli" = "PulseNet",
    "Camplylocbacter" = "PulseNet",
    "Listeria_monocytogenes" = "PulseNet"
  )
  # check first for WA ID if record already exists in db. if not assign the new anon ID to the new WA ID. 
  results <- results %>%
    rowwise() %>%
    mutate(
      anon_id = {
        if (any(c(
          is.na(wa_id), wa_id == "",
          is.na(collection_date), collection_date == "",
          is.na(Description), Description == ""
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
            prefix <- descriptor_prefixes[[Description]]
            
            if (is.null(prefix)) {
              msg <- paste("Unknown pathogen descriptor for WA ID:", wa_id, "-", Description, "→ skipping row.")
              warning(msg)
              log_message(msg)
              new_anon_id <- NA_character_
              break
            }
            
            new_anon_id <- paste0(prefix, "/Human/USA/WA-PHL-", rand_num, "/", year)
            
            # ensure that the new anon ID doesn't accidentally match another ID already in 
            existing <- dbGetQuery(con, paste0(
              "SELECT 1 FROM anon_ids WHERE anon_id = '", new_anon_id, "'"
            ))
            
            if (nrow(existing) == 0) {
              # Insert new identifier into DuckDB
              dbExecute(con, "INSERT INTO anon_ids (wa_id, anon_id, collection_date, Description) VALUES (?, ?, ?, ?)",
                        params = list(wa_id, new_anon_id, year, Description)
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
  
  dbDisconnect(con)
  return(results)
}


#function to drop the WA ID to create a df that can be exported to use for NCBI upload 
export_metadata <- function(results) {
  anon_id_clean <- results %>%
    select(-wa_id) %>%
    rename(sample_name = anon_id) %>%
    mutate(ncbi_bioproject = "PRJNA288601",
           title = NA_character_,
           description = NA_character_,
           authors = "list of AMD wet and dry lab plus Philip?",
           submitting_lab = "Washington State Department of Health Public Health Laboratories",
           submitting_lab_division = "Division of Disease Control & Health Statistics",
           isolate = sample_name,
           host_disease = NA_character_,
           organism = Description,
           lat_long = NA_character_,
           source_type = NA_character_,
           strain = NA_character_,
           purpose_of_sampling = NA_character_,
           assembly_protocol = "SPAdes",
           mean_coverage = NA_character_, 
           fasta_path = NA_character_,
           gff_path = NA_character_,
           ncbi_sequence_name_sra = sample_name,
           illumina_sequence_instrument = "Illumina Miseq",
           illumina_library_source = "GENOMIC",
           illumina_library_strategy = "WGS",
           illumina_library_layout = "PAIRED",
           illumina_library_protocol = "Illumina DNA PREP KIT",
           illumina_sra_file_path1 = NA_character_,
           illumina_sra_file_path2 = NA_character_
    )
           
           
           
      

csv_filename <- file.path(Sys.getenv("EXPORT_PATH"), paste0("anon_metadata_", Sys.Date(), ".csv"))
write.csv(anon_id_clean, csv_filename, row.names = FALSE)
cat("Exported to:", csv_filename, "\n")

}

#execute functions
results <- assign_anon_ids(results, db_path, lock_path)
snapshot_to_parquet(results, output_dir = snapshot_dir)
export_metadata(results)
#delete rds file?