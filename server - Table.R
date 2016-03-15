library(shiny)
library(plyr)
library(ggplot2)
library(xts)
library(RMySQL)

shinyServer(function(input, output) {

library(ggplot2)
library(RMySQL)
  
  conn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!',
                    host='mysql.crowd-scout.net', db='nhl_all')
  on.exit(dbDisconnect(conn))

  current_elo <- dbGetQuery(conn, "SELECT a.player_name, ROUND( elo, 1 ) AS elo, elo_trend2, c.pos, 
      c.team, round(DATEDIFF(current_date(),DOB)/365.25,1) as age, 
      c.height, c.weight, draft_team, draft_oa, draft_year
                            FROM  `hockey_elo_v1` AS a
                            INNER JOIN (
                            
                            SELECT player_id, MAX(  `order` ) AS last_game
                            FROM  `hockey_elo_v1` 
                            GROUP BY player_id
                            )b ON a.player_id = b.player_id
                            AND a.`order` = b.last_game
                            
                            LEFT JOIN 
                            
                            (SELECT player_id, sum(ELO_DIFF) as elo_trend,  round(sum(ELO_DIFF),1) as elo_trend2
                            FROM  
                            ((select player_id1 as player_id, player_name, sum( `curr_elo_1` - `prior_elo_1` ) as ELO_DIFF, max(`game_ts`) as last_game
                            FROM `hockey_games_v1` as a
                            LEFT JOIN `hockey_roster_v1` as b
                            ON a.player_id1 = b.nhl_id
                            WHERE game_ts >= NOW() - INTERVAL 1 WEEK
                            and player_name is not null
                            GROUP BY 1,2) 
                            UNION ALL
                            (select player_id2 as player_id,player_name, sum( `curr_elo_2` - `prior_elo_2` ) as ELO_DIFF,  max(`game_ts`) as last_game 
                            FROM `hockey_games_v1` as c
                            LEFT JOIN `hockey_roster_v1` as d
                            ON c.player_id2 = d.nhl_id
                            WHERE game_ts >= NOW() - INTERVAL 1 WEEK
                            and player_name is not null
                            GROUP BY 1,2)) as x
                            Group by player_id, player_name) d
                            
                            ON a.player_id = d.player_id
                            
                            LEFT JOIN hockey_roster_v1 AS c ON a.player_id = c.nhl_id
                            
                            LEFT JOIN hockey_draft_v0 as e
                            on a.player_id= e.player_id
                            ORDER BY elo DESC")
  

  output$team <- renderUI({
    selectInput("team", 
                "Team:", 
                c("All", 
                  as.list(sort(unique(current_elo$team)))))
  })
  
  output$draft_year <- renderUI({
    selectInput("draft_year", 
                "Draft Year:", 
                c("All", 
                  as.list(sort(unique(current_elo$draft_year)))))
  })
  
  output$position <- renderUI({
    selectInput("pos", 
                "Position:", 
                c("All", 
                  as.list(sort(unique(current_elo$pos)))))
  })  

  output$table <- renderDataTable({
   
     data <- current_elo
     
    if (input$team != "All"){
      data <- data[data$team == input$team,]
    }
    if (input$pos != "All"){
      data <- data[data$pos == input$pos,]
    }
    if (input$draft_year != "All"){
      data <- data[data$draft_year == input$draft_year,]
    }
    
    names(data) <- c("Player", "Current Elo","Trend Elo (Week)", "Position", "Team", "Age", "Height","Weight","Draft Team","Draft Overall","Draft Year")
    
    data
  })
  
  
})