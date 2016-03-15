library(shiny)
library(plyr)
library(ggplot2)
library(xts)
library(RMySQL)

##getConnection <- function() {
##  if (!exists('conn', where=.GlobalEnv) || (Sys.time() - .connOpened) > 600) {
##    if (exists('conn', where=.GlobalEnv))
##      closeConnection(.connection)
##    .connection <<- openNewConnection()
##    .connOpened <<- Sys.time()
##  }
##  return(conn)
##}

shinyServer(function(input, output) {

library(ggplot2)
library(RMySQL)
library(dplyr)
  
  conn <- dbConnect(MySQL(), user='ca_elo_games', password='xxxx!',
                    host='mysql.crowd-scout.net', db='nhl_all')
  on.exit(dbDisconnect(conn))

  player_elo <- dbGetQuery(conn, "SELECT Player, GM_DATE as Date, avg(Elo) as Elo
								                  FROM hockey_daily_elo
                                  GROUP BY 1,2")
  
  
  
  player_list <- as.list(unique(player_elo$Player))

  last_elo <- merge( summarize(group_by(player_elo,Player), Date=max(Date) ), player_elo, by=c("Player","Date"), all.x = TRUE)
  last_elo$Date <- as.character(Sys.Date())
  
  player_elo <- full_join(player_elo,last_elo,by=c("Player","Date","Elo"), all=TRUE)
  
    
  output$p1sel <- renderUI({
    selectInput("p1", 
                label = "Player 1:",
                choices = c(player_list),
                selected = "Erik Karlsson")
  })
  
  output$p2sel <- renderUI({
    selectInput("p2", 
                label = "Player 2:",
                choices = c(player_list),
                selected = "Drew Doughty")
  })
  

  output$myplot <- reactivePlot(function() {

    p_select <- subset(player_elo, Player %in% c( input$p1 , input$p2 ) & Date >= input$start_dt )

    p <- function(data){
      
        p=ggplot(data, aes(x=Date, y=Elo, group = Player, linetype = Player, color = Player)) +
          geom_smooth() +
          ggtitle("Player Elo Ratings") + 
          theme(plot.title = element_text(lineheight=.7, face="bold") ) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          theme(legend.position="bottom", legend.text=element_text(size=14), legend.title=element_blank())
        
    }    
      
    plot=p(p_select)   
    print(plot)
  })
  
})
