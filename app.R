library(shiny)
library(DBI)
library(duckdb)
library(DT)
library(readr)

# Read DuckDB path from .Renviron
db_path <- Sys.getenv("DUCKDB_PATH")

# UI
ui <- fluidPage(
  titlePanel("Query Seq IDs DuckDB"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("id_type", "Search by:", choices = c("wa_id", "anon_id")),
      textAreaInput("id_input", "Paste Sample ID(s):", 
                    placeholder = "Enter one ID per line or comma-separated"),
      fileInput("file_upload", "Or upload a file of IDs (.csv or .txt)", 
                accept = c(".csv", ".txt")),
      actionButton("query_btn", "Run Query"),
      br(), br(),
      downloadButton("download_btn", "Download Results")
    ),
    
    mainPanel(
      DTOutput("results_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  results <- reactiveVal(data.frame())
  
  observeEvent(input$query_btn, {
    # Connect to DuckDB in read-only mode
    con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
    on.exit(dbDisconnect(con), add = TRUE)
    
    ids <- NULL
    
    # Handle pasted input
    if (nzchar(input$id_input)) {
      ids <- unlist(strsplit(input$id_input, "[,\\s]+"))
    }
    
    # Handle file upload (if any)
    if (!is.null(input$file_upload)) {
      ext <- tools::file_ext(input$file_upload$name)
      if (ext == "csv") {
        df <- read_csv(input$file_upload$datapath, show_col_types = FALSE)
        ids <- unique(na.omit(unlist(df[[1]])))  # Take first column
      } else if (ext == "txt") {
        ids <- readLines(input$file_upload$datapath, warn = FALSE)
      }
    }
    
    # Validate input
    if (is.null(ids) || length(ids) == 0) {
      showNotification("No valid IDs found.", type = "error")
      return(NULL)
    }
    
    # Sanitize and quote IDs
    id_column <- input$id_type
    id_list <- paste(shQuote(ids, type = 'sh'), collapse = ", ")
    
    query <- paste0(
      "SELECT wa_id, anon_id, collection_date, Description FROM anon_ids WHERE ",
      id_column, " IN (", id_list, ")"
    )
    
    # Run query
    df <- dbGetQuery(con, query)
    results(df)
  })
  
  # Render results
  output$results_table <- renderDT({
    req(results())
    datatable(results(), options = list(pageLength = 25))
  })
  
  # Download handler
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0("query_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
