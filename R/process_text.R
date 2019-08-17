# process text through extract test stats and statcheck if requested

processText <- function(paper_text_tibble, statcheck = F) {
  # This function takes a list of the paper's paragraphs, and runs the extraction function on each
  # Note that it expects the list to take a specific form - as produced 
  # If statcheck = T, it also runs statcheck on the file
  
  # processing all but the PMID with extract test stats
  paper_text_list <- as.list(paper_text_tibble[c("names", "text")])
  
  statisticalOutput <-
    apply(data.frame(unlist(paper_text_list[1]), unlist(paper_text_list[2]), stringsAsFactors = F), 1,
          function(x) {
            extractTestStats(x[2], sectionName = x[1], context = T)
          })
  
  # NA rows removed here using a filter:
  notNAs <- unlist(lapply(statisticalOutput, function(x) !is.na(x[[2]][1])))
  
  if (any(notNAs)) {
    statisticalOutput <-
      data.frame(PMCID = paper_text_tibble$PMCID[[1]],
                 dplyr::bind_rows(statisticalOutput[notNAs]), 
                 stringsAsFactors = F)
  } else {
    statisticalOutput <- NA
  }
  
  if(statcheck == TRUE) {
    statCheckOutput <- lapply(output$text[-1], function(x) {
      if (length(x) > 0) {
        if (is.na(x)) {
          return(NA)
        } else{
          statcheck::statcheck(x)
        }
      } else
        NA
    })
    
    # NA rows removed here using a filter:
    notNAs <-
      unlist(lapply(X = statCheckOutput, FUN =  elementExists))
    if (any(notNAs)) {
      statCheckOutput <-
        data.frame(PMCID = paper_text_list$PMCID[[1]], dplyr::bind_rows(statCheckOutput[notNAs], .id = "section"))
    } else {
      statCheckOutput <- NA
    }
    return(list(statisticalOutput, statCheckOutput))
  }
  return(statisticalOutput)
}

