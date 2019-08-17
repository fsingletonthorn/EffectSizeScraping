# Download article from PMC and run all test extraction and search functions on
scrapePMC <- function(call, ftpCall, statcheck = T) {
  pulledPMC <-  pullPMC(call = call)
  
  # Checking if we've got more than just the abstract
  if ((length(unlist(pulledPMC$text)) == 4) |
      sum(stringr::str_count(as.character( pulledPMC$text ), " ")) < 500) {
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
    if (length(pdfLoc) > 0) {
      # Always only work on the first PDF, just in case there is more than 1
      # This also binds in the PMC number, although it excludes
      extractedText <- extractPdf(pdfLoc[[1]])
      extractedText$PMCID <- pulledPMC$metadata$PMCID
      # test if the extracted text is longer than that already stored
      if( sum(stringr::str_count(as.character(extractedText), " ")) >  sum(stringr::str_count(as.character( pulledPMC$text ), " "))) {
      pulledPMC$text <-
        rbind(pulledPMC$text,  extractedText)
      }
    }  
      # Cleanup
      unlink( c( exdir, tempLoc ), recursive = T)
  }
     pulledPMC$statisticalTests <- processText(pulledPMC$text)
     
     return(pulledPMC)
}
