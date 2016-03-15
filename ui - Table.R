library(shiny)
library(ggplot2)

shinyUI(
  fluidPage(
    h5('Scout at http://www.crowdscoutsports.com'),
    h5('Follow @crowdscoutsprts'),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4, 
             uiOutput('team')
      ),
      column(4, 
             uiOutput('draft_year')
      ),
      column(4, 
             uiOutput('position') 
      )        
    ),
    ###row 2
##    fluidRow(
##     column(4, 
##             sliderInput("age", "Age (16-Jagr):",
##                         min = 16, max = 50, value = c(16,50))
##      ),
##      column(4, 
##             sliderInput("Draft Year", 
##                         "Draft Year:", 
##                         min = 1990, max = 2015, value = c(1990,2015))
##      ),
##      column(4, 
##             sliderInput("elo", 
##                         "Current Elo:", 
##                         min = 1000, max = 2000, value = c(1000,2000))
##      )        
##    ),  
    # Create a new row for the table.
    fluidRow(
      dataTableOutput(outputId="table")
    )    
  )  
)