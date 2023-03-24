#Import libraries
library(shiny)
library(ggplot2)
library(DT)
library(tidyverse)
library(rsconnect)

rm(list = ls())

setwd("~/DATA-332/march_madness/marchmadness2023/Files for Analysis")

#Read data
data_df <- read.csv("2023_game_data.csv")
column_names<-colnames(data_df) #for input selections

#Define ui to draw a histogram

ui <- fluidPage(
  # App title ----
  titlePanel(title = "March Madness 2023 Dataset"),
  fluidRow(
    column(2,
           selectInput('X', 'Choose Seed',column_names,column_names[2]),
           selectInput('Y', 'Choose Kenpom Adjusted Efficiency',column_names,column_names[6]),
           selectInput('Z', 'Choose Barttorvik Adjusted Efficiency',column_names,column_names[10]),
           selectInput('Splitby', 'Split By', column_names,column_names[3])
    ),
    column(4,plotOutput('plotMarchMadness')),
    column(6,DT::dataTableOutput("table_01", width = "100%"))
  )
)


server<-function(input,output){
  
  output$plotMarchMadness <- renderPlot({
    
    ggplot(data_df, aes_string(x=input$X, y= input$Y, z= input$Z))+
      geom_point()+
      geom_smooth()
  })
  output$table_01<-DT::renderDataTable(data_df[,c(input$X,input$Y,input$Z, input$Splitby)],options = list(pageLength = 4))
  
}

shinyApp(ui=ui, server=server)


