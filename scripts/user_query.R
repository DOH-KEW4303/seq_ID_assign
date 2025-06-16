##########################
#This script will allow the user to access the DuckDB file in read-only mode. they can use the .Renviron file or manually set the path to the file?

# Load necessary libraries
library(DBI)
library(duckdb)

# Use env variable to set the path 
db_path <- Sys.getenv("DUCKDB_PATH") 

# Connect to DuckDB in read-only mode
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

# Define WA ID to search, replace in quotes 
wa_id_to_search <- "WA0123456"

# Search the anon_ids table
query <- paste0("SELECT * FROM anon_ids WHERE wa_id = '", wa_id_to_search, "'")

result <- dbGetQuery(con, query)
print(result)

# List tables in the database
# tables <- dbListTables(con)
# print(tables)
# example: Query one table to inspect contents, this just shows top 5 records
# if ("anon_ids" %in% tables) {
#   sample_query <- dbGetQuery(con, "SELECT * FROM anon_ids LIMIT 5")
#   print(sample_query)
# }

# Disconnect when done
dbDisconnect(con, shutdown = TRUE)
