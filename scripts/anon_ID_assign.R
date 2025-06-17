#################################
#Reads in rds file with lims query results, connects to duckdb and executes function to assign random identifier to wach WA record. writes to duckdb file on the y drive. 


# read in rds file with lims query results, Get path from .Renviron for duckdb file
results <- readRDS(file = file.path(secure_path, "lims_query_results.rds"))
db_path <- Sys.getenv("DUCKDB_PATH")

# Create/connect to the DuckDB file and create table
assign_anon_ids <- function(results, db_path) {
  # Connect to duckdb (read-only = FALSE because we're inserting)
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  
  # Create table if it doesn't exist yet
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS anon_ids (
      wa_id TEXT PRIMARY KEY,
      anon_id TEXT,
      collection_date INTEGER
    )
  ")
  # Loop through results df and assign anon_id if not already in DB
  results<- results %>%
    rowwise() %>%
    mutate(
      anon_id = {
        existing_id <- dbGetQuery(con, paste0(
          "SELECT anon_id FROM anon_ids WHERE wa_id = '", wa_id, "'"
        ))
        
        if (nrow(existing_id) > 0) {
          existing_id$anon_id
        } else {
          # Generate new ID
          year <- year(as.Date(collection_date))
          repeat {
            rand_num <- sprintf("%06d", sample(1e6, 1))
            new_anon_id <- paste0("WAPHL/Homo sapiens/USA/WA-PHL-", rand_num, "/", year)
            
            # Check if random ID is already used
            existing <- dbGetQuery(con, paste0(
              "SELECT 1 FROM anon_ids WHERE anon_id = '", new_anon_id, "'"
            ))
            
            if (nrow(existing) == 0) {
              # Insert into DuckDB
              dbExecute(con, "INSERT INTO anon_ids (wa_id, anon_id, collection_date) VALUES (?, ?, ?)", 
                        params = list(wa_id, new_anon_id, year))
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

#call the function to assign ID's 
results <- assign_anon_ids(results, db_path)
view(results)

#delete rds file?