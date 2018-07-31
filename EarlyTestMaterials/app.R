library(shiny)
library(randomNames)
library(tidyverse)
library('DT')

# note that this is a mock up, data is not real, and names, years etc. are made up on the fly


# unlist(lapply(data[[4]][filter], length))

jList <- c("Nature", "Science", "PLOSOne", "Ruminant Science")
effectSizes <- list()
Journal <- sample(jList, 1200, replace =T)
Year <- sample(1950:2018, 1200, replace = T)
Names <- list()
for(i in 1:1200) {
  Names[[i]] <- randomNames(round(runif(1, min = 1, max = 5)))
}
for(i in 1:1200) {
  effectSizes[[i]] <- rnorm(round(runif(1, min = 0, max = 12)), rnorm(1,.5,.5), abs(rnorm(1,.5,.5))) 
}
effectSizeTypes<- sample(c("r", "d", "eta"), 1200, replace = T)
data<-list(Journal, Year, Names, effectSizes, effectSizeTypes)


ui <- fluidPage(
  titlePanel("Effect Sizes"), 
  sidebarLayout(
    sidebarPanel(
      radioButtons("ESTypeInput", label = "Effect size type",
                   choices = c(unique(effectSizeTypes)),
                   selected = "r", width = '100%', inline = F),
      br(),
      selectInput("JournalInput", label = "Journal",
                   choices = c(unique(Journal), "All"),
                   selected = "All journals"),
      br(),
      sliderInput("YearInput", "Year range",value = c(1950, 2018), min = 1950, max = 2018),
      br(), br(),
      textAreaInput("KeywordFilter", label = "Keyword Filter *"),
      textAreaInput("AuthorFilter", label = "Author Filter *"),
      downloadButton("DownloadFiltered", label = "Download")
      ),
          mainPanel(
        plotOutput("coolplot"),
        br(), br(),
        dataTableOutput("results") 
        )
  )
)



server <- function(input, output) {
 
  output$coolplot <- renderPlot({
    filter <- which(data[[5]] == input$ESTypeInput & 
                      data[[1]] == input$JournalInput &
                      data[[2]] >= input$YearInput[1] &  data[[2]] <= input$YearInput[2])
    filtered <- data.frame(unlist(data[[4]][filter]))
    names(filtered) <- "ES"
    ggplot(filtered, aes(ES)) +
      geom_histogram(bins = 30) + xlab(input$ESTypeInput) +
      theme_bw()
  })
  output$results <- renderDataTable({
    filter <- which(data[[5]] == input$ESTypeInput & 
                      data[[1]] == input$JournalInput &
                      data[[2]] >= input$YearInput[1] &  data[[2]] <= input$YearInput[2])
  
    filtered <- data.frame(unlist(data[[4]][filter]))
    ns<-unlist(lapply(data[[4]][filter], length))
    names(filtered) <- "ES"
    filtered$Authors <- randomNames(nrow(filtered))
    filtered$Journal <- input$JournalInput
    filtered$year <- round(runif(nrow(filtered), input$YearInput[1], input$YearInput[2]))
    filtered
    })
}

shinyApp(ui = ui, server = server)
