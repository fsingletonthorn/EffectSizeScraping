library(RSQLite)
library(tidyverse)
#library(RPostgreSQL)

## Alternative using postgreSQL if required later:
# drv <- dbDriver("PostgreSQL"); pw <- "censored"
# con  <- dbConnect(drv, dbname = "effectSizeScraping",
#                   host = "localhost", port = 5432,
#                   user = "openpg", password = pw)

saveOutput <- function(output) {
  # my_db <- src_sqlite(my_db_file, create = TRUE)
  my_db_file <- "database/database.sqlite"
  
  # drv <- dbDriver("PostgreSQL")
  con  <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
  
  dbExistsTable(con, name = "keywords")
  
  # Check to make sure that the DOI doesn't exist before updating tables, then update
  if(dbGetQuery(con, statement = paste0("SELECT COUNT(1) FROM metadata WHERE PMCID = '",  output$metadata$PMCID, "'")) >= 1)
  {
    return(stop("PMCID already exists in database"))
  } else {
    dbWriteTable(
      con,
      name = "metadata",
      value = output$metadata,
      row.names = FALSE,
      append = TRUE
    )
    
    dbWriteTable(
      con,
      name = "authors",
      value = output$authors,
      row.names = FALSE,
      append = TRUE
    )
    
    dbWriteTable(
      con,
      name = "keywords",
      value = output$keywords,
      row.names = FALSE,
      append = TRUE
    )
    
    dbWriteTable(
      con,
      name = "statistics",
      value = output$statisticalOutput,
      row.names = FALSE,
      append = TRUE
    )
    
    dbWriteTable(
      con,
      name = "statcheck",
      value = output$statisticalOutput,
      row.names = FALSE,
      append = TRUE
    )
  }
  dbReadTable(con, name = "statistics")
}
