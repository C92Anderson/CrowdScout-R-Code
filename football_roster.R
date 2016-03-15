##install.packages("rvest")
##install.packages("dplyr")
##install.packages("RCurl")
##install.packages("foreach")

library(dplyr)
library(RCurl)
library(rvest)
library(foreach)

###team abbreviations
team_xwalk <- data.frame(matrix(c('atlantafalcons','ATL',
                                  'baltimoreravens','BAL',
                                  'minnesotavikings','MIN',
                                  'Sabres','BUF',
                                  'Hurricanes','CAR',
                                  'BlueJackets','CBJ',
                                  'Flames','CGY',
                                  'Blackhawks','CHI',
                                  'Avalanche','COL',
                                  'Stars','DAL',
                                  'RedWings','DET',
                                  'Oilers','EDM',
                                  'Panthers','FLA',
                                  'Kings','LAK',
                                  'Canadiens','MTL',
                                  'Devils','NJD',
                                  'Predators','NSH',
                                  'Islanders','NYI',
                                  'Rangers','NYR',
                                  'Senators','OTT',
                                  'Flyers','PHI',
                                  'Penguins','PIT',
                                  'Sharks','SJS',
                                  'Blues','STL',
                                  'Lightning','TBL',
                                  'MapleLeafs','TOR',
                                  'Canucks','VAN',
                                  'Jets','WPG',
                                  'Capitals','WSH'),ncol=2, byrow=TRUE))
colnames(team_xwalk) <- c("longteam","tm")

football_roster <- data.frame(no=character(),
                         player_name=character(),
                         pos=character(),
                         status=character(),
                         height=character(),
                         weight=integer(),
                         dob=character(),
                         exp=integer(),
                         school=character(),
                         team=character(), 
                              stringsAsFactors=TRUE)

teams <- c("ATL","BAL","MIN")

for(i in teams){  

team_details <- team_xwalk[which(team_xwalk$tm == i), ]
  
url <- read_html(print(paste0("http://www.nfl.com/teams/",team_details$longteam,"/roster?team=",team_details$tm)))

roster <- html_nodes(url, "td")
roster1 <- matrix(html_text(roster)[c(2:length(roster))], byrow = TRUE, ncol = 9) %>% data.frame()

colnames(roster1) <- c("no","player_name","pos","status","height","weight","dob","exp","school")

##assign team name
roster1$team <- print(paste(i))

##append data
football_roster <- rbind(football_roster, roster1)

}

##clean player name
football_roster$player_name <- gsub("'","",football_roster$player_name)

##clean height
football_roster$height <- gsub('"',"",football_roster$height)

###set birthdate
football_roster$dob <- as.Date(as.Date(football_roster$dob, format='%m/%d/%Y'), origin="1970-01-01")

football_roster$dob1 <- as.Date(as.Date(football_roster$dob, format=6.0), origin="1970-01-01")


##create player name xwalk
football_roster$pnm <- toupper(gsub(" ","",football_roster$player_name))
football_roster$pnm <- gsub("[^A-Za-z]", "", football_roster$pnm)


###export
write.csv(football_roster,file="~/CWA/football_roster.csv",row.names = FALSE)
