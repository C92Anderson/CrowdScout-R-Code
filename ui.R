library(shiny)
library(ggplot2)

shinyUI(fluidPage(

  plotOutput('myplot'),
  
  fluidRow(
    column(5, offset = 1,
          h5('Compare Daily Elo Ratings of Selected Players'),
       ##       br(),
              uiOutput('p1sel'),
              uiOutput('p2sel')
          
           ),
    column(5, offset = 1,
           dateInput('start_dt',
                     label = paste('Start Date:'),
                     value = Sys.Date() -90),
           br(),
           h5('Scout at http://www.crowdscoutsports.com'),
           h5('Follow @crowdscoutsprts'))
          )
  
    
## headerPanel('Player Compare Tool'),

##  sidebarPanel(
##  
##    helpText('Compare Daily Elo Ratings of Selected Players'),
##    
##    uiOutput('p1sel'),
##    uiOutput('p2sel'),
    
##    helpText('Scout at http://www.crowdscoutsports.com'),
##    helpText('Follow @crowdscoutsprts'),
    
##    dateInput('start_dt',
##              label = paste('Start Date:'),
##              value = Sys.Date() -40
##    )
    
#3  ),
##    mainPanel(plotOutput('myplot')
##    )
     
))