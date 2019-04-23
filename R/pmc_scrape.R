
# Download article from PMC

# Pull PMC

# articles[articles$PMID == ]

# Example sans text 
# pmcID <- 4710337
# call <- articles$oaiCall[articles$PMCID == pmcID]
# ftpCall  <- articles$tpfCall[articles$PMCID == pmcID]

scrapePMC <- function(call, ftpCall, statcheck = F) {
  pulledPMC <-
    pullPMC(call = articles$oaiCall[articles$PMCID == pmcID])
  
  #
  if ((length(pulledPMC$text) == 2)) {
    # To check if abstract is NA  add "& is.na(textTemp$text[2,2][[1]])"
    # articles$tpfCall[articles$PMCID == pmcID]
    
    # Create temp file location
    tempLoc <- tempfile()
    # download from ftp file 
    download.file(ftpCall, destfile = tempLoc, quiet = T)
    # (ftpCall, httr::write_disk(tempLoc, overwrite = T))
    # Setting extraction directory
    exdir <- paste0(tempLoc, "_extracted")
    # Extracting file
    untar(tempLoc, exdir = exdir)
    # Finding the PDF file
    pdfLoc <-
      list.files(
        exdir,
        recursive = T,
        full.names = T,
        pattern = ".*\\.pdf"
      )
    
    # If PDF file exists
    if (length(pdfLoc > 0)) {
      # Always only work on the first PDF, just in case there is more than 1
      # This also binds in the PMC number, although it excludes
      pulledPMC$text <-
        rbind(pulledPMC$text,  extractPdf(pdfLoc[[1]]))
      
      pulledPMC$text <-  processPMC(pulledPMC$text)
      
      # Cleanup
      unlink( c( exdir, tempLoc ), recursive = T)
      
      return(pulledPMC)
      
    } else {
      pulledPMC$text <- processPMC(pulledPMC$text)
      return(pulledPMC)
    }
    
  }
}



# Process text file
# processPMC()

# Check for 95% CIs
# Check for PA 
# Check for . . .
