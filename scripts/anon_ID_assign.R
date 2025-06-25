#################################
#Reads in rds file with lims query results, connects to duckdb and executes function to assign random identifier to wach WA record. writes to duckdb file on the y drive. 


# read in rds file with lims query results, Get path from .Renviron for duckdb file and lock file 
results <- readRDS(file = file.path(secure_path, "lims_query_results.rds"))
db_path <- Sys.getenv("DUCKDB_PATH")
lock_path <- Sys.getenv("LOCK_PATH")

#check for exisiting lock file and display info if present
if(file.exists(lock_path)) {
  lock_info <- readLines(lock_path, warn = FALSE)
  stop(paste0(
    "The database file is currently locked / in use.\n",
    "Locked by:" , lock_info[1], "\n",
    "since: ", lock_info[2], "\n",
    "Try again later."
    ))
}
#create the user and timestamp info for lock
lock_info <- c(Sys.info()[["user"]], as.character(Sys.time()))
writeLines(lock_info, lock_path)

#remove lock file after process is finished
on.exit({
  if (file.exists(lock_path)) {
    file.remove(lock_path)
  }
}, add = TRUE)

# Create/connect to the DuckDB file and create table and write function to assign ID's 
assign_anon_ids <- function(results, db_path) {
  # Connect to duckdb (read-only = FALSE because we're inserting)
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  
  # Create table if it doesn't exist yet (ours will already exist)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS anon_ids (
      wa_id TEXT PRIMARY KEY,
      anon_id TEXT,
      collection_date INTEGER,
      descriptor TEXT
    )
  ")
  # check first for WA ID if record already exists (avoids assigning a new anon ID to the same WA ID)
    rowwise() %>%
    mutate(
      anon_id = {
        if (any(is.na(c(wa_id, collection_date, descriptor)))) {
          warning(paste("Skipping row due to missing required value:", wa_id))
          return(NA_character_)
        }
        existing_id <- dbGetQuery(con, paste0(
          "SELECT anon_id FROM anon_ids WHERE wa_id = '", wa_id, "'"
        ))
        
        if (nrow(existing_id) > 0) {
          existing_id$anon_id
        } else {
          # extract year
          year <- year(as.Date(collection_date))
          #try until we get a unique anon ID
          repeat {
            rand_num <- sprintf("%06d", sample(1e6, 1)) #generate padded 6 digit no 
            print(rand_num)
            #create IDs
            new_anon_id <- case_when(
              descriptor == "influenza A" ~paste0("FluA/Human/USA/WA-PHL-", rand_num, "/", year),
              descriptor == "influenza B" ~paste0("FluB/Human/USA/WA-PHL-", rand_num, "/", year),
              descriptor == "cov2" ~paste0("hCov/Human/USA/WA-PHL-", rand_num, "/", year),
              descriptor %in% c("salmonella", "shigella") ~paste0("PulseNet/Human/USA/WA-PHL-", rand_num, "/", year),
              descriptor == "wastewater" ~paste0("WW/Metagenomic/USA/WA-PHL-", rand_num, "/", year),
              TRUE ~paste0("WAPHL/Human/USA/WA-PHL-", rand_num, "/", year)
           )
            
            
            # ensure that the anon ID doesn't accidentally match another ID already in db 
            existing <- dbGetQuery(con, paste0(
              "SELECT 1 FROM anon_ids WHERE anon_id = '", new_anon_id, "'"
            ))
            
            if (nrow(existing) == 0) {
              # Insert new identifier into DuckDB
              dbExecute(con, "INSERT INTO anon_ids (wa_id, anon_id, collection_date, descriptor) VALUES (?, ?, ?, ?)", 
                        params = list(wa_id, new_anon_id, year, descriptor)
              )
              break
            }
          }
          new_anon_id
        }
      }
    ) %>%
    ungroup()
  
  dbDisconnect(con)
  return(results)
}

#call the function  
results <- assign_anon_ids(results, db_path)
view(results)

#drop the WA ID to create a df that can be exported to use for NCBI upload 
anon_df_clean <- results %>%
  select(-wa_id) %>%
  mutate(
    export_timestamp = Sys.time()
  )

csv_filename <- file.path(Sys.getenv("EXPORT_PATH"), paste0("anon_metadata_", Sys.Date(), ".csv"))
write.csv(anon_df_clean, csv_filename, row.names = FALSE)
cat("Exported to:", csv_filename, "\n")

#delete rds file?