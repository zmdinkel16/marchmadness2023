#Import libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinydashboard)

#Read data
r64_current <- read.csv("2023_game_data.csv")

#Define ui to draw a histogram
ui<- fluidPage(
  #Add title
  headerPanel("March madness prediction!"),
  
  #sidebar panel with inputs and output
  sidebarLayout(
    #sidebar for input
    sidebarPanel(
      #Dropbox
      selectInput("Distribution", "Please select Distribution Type",
                  #Replace random with options
                  choices = c("Random","Random")),
      #Input: slider for the number of bins in the graph, use bins since I don't know what to keep
      sliderInput(inputId = "bins",
                  label = "Number of bins",
                  min = 1,
                  max = 60,
                  value = 20, 
                  step = 2)
      ),
    #Main pannel for displaying output, use plot value for now
    mainPanel(
      #Output Histogram
      plotOutput(outputId = "plot")
    )
  )
)

#Define server to draw a histogram
server<- function(input, output, session){
  
}
#Create shinny app
shinnyApp(ui, server)


