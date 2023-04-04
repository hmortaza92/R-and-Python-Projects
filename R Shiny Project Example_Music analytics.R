#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("caret", dependencies = TRUE)
#install.packages("bst", dependencies = TRUE)
#install.packages("plyr", dependencies = TRUE)
#install.packages("h2o", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("lattice", dependencies = TRUE)

library("caret")
library("bst")
library("plyr")
library("h2o")
library("ggplot2")
library("lattice"
)
library("tidyverse")
library("dplyr")
library("shiny")
#setwd("C:/Users/z2f10/Documents/Documents/Summer 2022 - MGMT 5900 - Using R for Analytics/Final Project/FinalProjMusic")
tr<-read.csv("songs_normalize.csv", header=TRUE, stringsAsFactors = TRUE)
genre<-unlist(strsplit(as.character(tr$genre),","))
genre<-str_trim(genre,side="left")
about<-"This app was developed by: Hanna Mortazavi"

calculate_analytics<-function(rec_ycol, rec_genre){
  #-------------------------------------------------
  #begin analytics
  #-------------------------------------------------
  genreDataSet<- tr %>%
    filter(tr$genre %in% rec_genre) 
  #s1 <- tr[,c(6,1:5,7:18)] # brings target to first column
  s1 <- genreDataSet[,c(6,1:5,7:18)] # brings target to first column
  
  #str(s1)
  # drop high cardinality categorical columns (artists, track) bc they create too many rows
  s2 = s1[,c(1,4:18)]
  
  # dummyvars not required in this dataset; proceeded to preprocess
  
  names(s2)[1] <- "y" 
  s3 = s2[,c(1,3:4,16,5:15)]
  preProcValues <- preProcess(s3[,5:ncol(s3)], method = c("center","scale"))
  s3 <- predict(preProcValues, s3)
  rm(preProcValues)
  
  # we want to give end user all the data possible to analyze popularity so we are not removing correlated items
  
  h2o.init(nthreads=12, max_mem_size="64g")
  
  y <- "y"                               
  x <- setdiff(names(s3), y)  
  s3 = as.h2o(s3)
  parts <- h2o.splitFrame(s3, 0.7, seed=99) 
  train <- parts[[1]]              
  test <- parts[[2]]
  
  # using best model
  rftrain <- h2o.randomForest(x, y, train)
  
  h2o.performance(rftrain, train)
  h2o.performance(rftrain, test)
  
  predrf = h2o.predict(object = rftrain, newdata = test)
  
  predresult = as.data.frame(predrf)
  test1 = as.data.frame(test)
  
  finalpredpop = cbind.data.frame(test1, predresult$predict)
  
  colnames(finalpredpop)[16] <- "predpop"
  
  return (finalpredpop)
  #finalValues = as.data.frame(finalpredpop)
  #return (finalValues)
  #-----------------------------------------------------------
  #end Analytics
  #-----------------------------------------------------------
  
}


#library(ggplot2)

ui <- fluidPage(
  headerPanel('Music Analytics'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(tr),selected = names(tr)[[6]]),
    selectInput('ycol', 'Y Variable', names(tr),selected = names(tr)[[7]]),
    selectInput('genre', 'Genre',unique(genre))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Analytics", plotOutput("plot1")),
      tabPanel("Tabular Output", dataTableOutput(outputId="text")),
      tabPanel("Predictions", dataTableOutput(outputId="analytics_final")),
      tabPanel("About", about)
      
    )
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    data.frame(tr[,input$xcol], tr[,input$ycol],tr$explicit,tr$year,tr$genre,tr$danceability, tr$energy, tr$key, tr$loudness, tr$mode, tr$speechiness, tr$acousticness, tr$instrumentalness, tr$liveness, tr$valence, tr$tempo,tr$popularity)              
    #allData <-tr
  })
  
  #  print (selectedData())
  selectedData2 <-reactive({
    genreDataSet<- selectedData() %>%
      
      filter(tr$genre %in% input$genre) 
    
  })
  selectedData3 <-reactive({
    genreDataSet<- tr %>%
      select (input$ycol, tr$explicit, tr$year, tr()$genre, tr$danceability, tr$energy, tr$key, tr$loudness, tr$mode, tr$speechiness, tr$acousticness, tr$instrumentalness, tr$liveness, tr$valence, tr$tempo)  %>%
      filter(tr$genre %in% input$genre) 
    
  })
  
  output$plot1 <- renderPlot({
    
    # selectedData %>%
    #   filter(genre==input$genre)
    
    ggplot(data=selectedData2(), aes(x=selectedData2()[,1], y=selectedData2()[,2])) + 
      geom_point() + #ggplot2 boxplot
      ggtitle("Plot of Songs")+ 
      xlab(input$xcol) +
      ylab(input$ycol) + 
      geom_smooth(method = "lm", se = FALSE)
    
  })
  output$text <- renderDataTable({
    (selectedData2())
  })
  output$analytics_final <- renderDataTable({
    (calculate_analytics(input$ycol, input$genre))
  })
  
  
}

shinyApp(ui = ui, server = server)