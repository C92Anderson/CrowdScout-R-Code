library(shiny)
library(plyr)
library(ggplot2)
library(xts)
library(RMySQL)

shinyServer(function(input, output) {

library(ggplot2)
library(RMySQL)
library(dplyr)
    
  fconn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!',
                     host='mysql.crowd-scout.net', db='football_all')
  on.exit(dbDisconnect(fconn))

  player_elo <- dbGetQuery(fconn, "SELECT Player, GM_DATE as Date, avg(Elo) as Elo
                           FROM football_daily_elo
                           GROUP BY Player, GM_DATE")
  

  player_list <- as.list(unique(player_elo$Player))
  
  last_elo <- merge( summarize(group_by(player_elo,Player), Date=max(Date) ), player_elo, by=c("Player","Date"), all.x = TRUE)
  last_elo$Date <- as.character(Sys.Date())
      
  player_elo <- full_join(player_elo,last_elo,by=c("Player","Date","Elo"), all=TRUE)
  
  
  output$p1sel <- renderUI({
    selectInput("p1", 
                label = "Player 1:",
                choices = c(player_list),
                selected = "Aaron Rodgers")
  })
  
  output$p2sel <- renderUI({
    selectInput("p2", 
                label = "Player 2:",
                choices = c(player_list),
                selected = "Tom Brady")
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