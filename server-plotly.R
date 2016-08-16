library(shiny)
library(plyr)
library(ggplot2)
library(xts)
library(RMySQL)
library(plotly)
options(shiny.deprecation.messages=FALSE)

  conn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!',
                    host='mysql.crowd-scout.net', db='nhl_all')
  on.exit(dbDisconnect(conn))
  
  daily_elo <- dbGetQuery(conn, "SELECT * from `nhl_all`.hockey_daily_elo_py")

  p <- ggplot(data = daily_elo, aes(x = Date, y = Score, group=Player, color=Player)) +
    geom_smooth(aes(colour = Player)) + 
    ylim(0,100) 
  
  (gg <- ggplotly(p))
  
  plot_ly(data = daily_elo[1:5,], x = Date, y = Score, name = "spline", line = list(shape = "spline"))
  
  
  
  
  player_list <- as.list(unique(player_elo$Player))

  last_elo <- merge( ddply(player_elo, "Player", function(z) tail(z,1)), player_elo[ ,-3], by=c("Player","Date"), all.x = TRUE)  
  last_elo$Date <- as.character(Sys.Date())
  
  
  player_elo <- full_join(player_elo,last_elo,by=c("Player","Date","Score"), all=TRUE)

  player_elo$Date <- as.Date(player_elo$Date)

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

    p_select <- subset(player_elo, Player %in% c( input$p1 , input$p2 ) )

    p <- function(data){
      
        p=ggplot(data, aes(x=Date, y=Score, group = Player, linetype = Player, color = Player)) +
          geom_smooth() +
          ggtitle("Player Daily CS Score") + 
          labs(y = "Player CS Score (0-100)") +
          theme(plot.title = element_text(lineheight=.7, face="bold") ) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          ylim(0, 100) +
          scale_x_date( labels = date_format("%d %b %Y") , breaks = date_breaks("week"), limits = c(input$start_dt, Sys.Date()))  +
          theme(legend.position="bottom", legend.text=element_text(size=14), legend.title=element_blank())
        
    }    
      
    plot=p(p_select)   
    print(plot)
  })
  
})