  ##install.packages("rvest")
  ##install.packages("dplyr")
  ##install.packages("RCurl")
  ##install.packages("foreach")
  
  library(dplyr)
  library(RCurl)
  library(rvest)
  library(foreach)
  
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
  
  teams <- c('ATL','MIN','BUF','MIA','NE','NYJ','BAL','CIN','CLE','PIT','HOU','IND','JAX','TEN','DEN','KC','OAK','SD','SEA','SF','RAM','ARZ','TB','NO','CAR',
             'DAL','NYG','PHI','WAS','CHI','DET','GB')
  
  
  for(i in teams){  
  
  url <- read_html(print(paste0("http://www.ourlads.com/nfldepthcharts/roster/",i)))
    
    ###roster text list
    roster.text <- url %>% 
      html_nodes("td") %>% 
      html_text()                                 
    
    ###roster df
    roster.df <- roster.text[!toupper(roster.text) %in% c("RESERVES","ACTIVE PLAYERS","UN-SIGNED PLAYERS")] %>%
      matrix(byrow=TRUE, ncol=11) %>% 
      data.frame()
    
  colnames(roster.df) <- c("no","player_name","pos","dob","age","height","weight","school","draft_team","draft_raw","experience")
  
  ##assign team name
  roster.df$team <- print(paste(i))
  
  ##append data
  football_roster <- rbind(football_roster, roster.df)
  
  }
  
  ##clean player name
  football_roster$player_name <- gsub("[.]","",football_roster$player_name)
  football_roster$player_name <- gsub("'","",football_roster$player_name)
  football_roster$player_name <- gsub("\\(.+?\\)","",football_roster$player_name)
  football_roster$player_name <- gsub("\\b[a-zA-Z0-9]{1}\\b", "", football_roster$player_name) 
  
  
  ##clean height
  football_roster$height <- gsub('"',"",football_roster$height)
  
  ###set birthdate
  football_roster$dob <- as.Date(as.Date(football_roster$dob, format='%m/%d/%Y'), origin="1970-01-01")
  
  football_roster$dob1 <- as.numeric(as.Date(football_roster$dob, origin="1900-01-01")) + 25569
  
  
  ##create player name 
  football_roster$last.name <- sapply(strsplit(football_roster$player_name,", "), function(x) x[1])
  football_roster$first.name <- sapply(strsplit(football_roster$player_name,", "), function(x) x[2])
  football_roster$player_name <- paste0(football_roster$first.name," ",football_roster$last.name)
  
  ###create cs_id
  football_roster$draft_year <- as.numeric(sapply(strsplit(as.character(football_roster$draft_raw)," "), function(x) x[1]))
  football_roster$draft_oa <- sapply(strsplit(as.character(football_roster$draft_raw)," "), function(x) x[length(x)])
  football_roster$draft_team <- gsub(" ","",substr(football_roster$draft_team, 1, 3))
  
  ##fix franchises
  football_roster$draft_team[football_roster$draft_team=="BA"] <- "BAL" ##mistake with baltimore 
  football_roster$draft_team[football_roster$draft_team=="SL"] <- "LA" ##same franchise
  football_roster$team[football_roster$team=="RAM"] <- "LA" ##same franchise
  
  ##fix draft year
  football_roster$draft_year <- ifelse(football_roster$draft_year<50,
                                       2000+football_roster$draft_year,
                                       1900+football_roster$draft_year)
  ##Fix OA 
  football_roster$draft_oa[football_roster$draft_oa=="01"] <- "012" ##Ryan Matthews 
  football_roster$draft_oa[football_roster$draft_oa=="Supp"] <- "upp" ##Supp Draft 
  
  ##new draft_info
  football_roster$draft_info <- ifelse(is.na(as.numeric(football_roster$draft_oa)),
                            paste0(football_roster$draft_oa," (",football_roster$draft_year," ",football_roster$draft_team,")"),
                            paste0(as.numeric(football_roster$draft_oa)," OA (",football_roster$draft_year," ",football_roster$draft_team,")"))
                            
  ##create cs_id
  football_roster$cs_id <- paste0(as.numeric(as.Date(football_roster$dob, origin="1900-01-01")) + 25569,
                                  toupper(substr(football_roster$last.name,1,3)),
                                  substr(football_roster$draft_oa,1,3))
  
  ##clean pos
  football_roster$meta_pos <- sapply(strsplit(as.character(football_roster$pos),"/"), function(x) x[1])
  football_roster$meta_pos <- ifelse(grepl("QB", football_roster$meta_pos, perl=TRUE),"Passers",
                                    ifelse(grepl("RB|FB|HB", football_roster$meta_pos, perl=TRUE),"Runners",
                                        ifelse(grepl("WR|TE", football_roster$meta_pos, perl=TRUE),"Receivers",
                                            ifelse(grepl("OT|OG|OC", football_roster$meta_pos, perl=TRUE),"Blockers",
                                                ifelse(grepl("DE|DT|NT", football_roster$meta_pos, perl=TRUE),"Front7",
                                                    ifelse(grepl("IB|ILB|LB|MLB|OB|OLB", football_roster$meta_pos, perl=TRUE),"Front7",
                                                        ifelse(grepl("CB|FS|SS|S", football_roster$meta_pos, perl=TRUE),"Secondary",
                                                           ifelse(grepl("PK|P|LS", football_roster$meta_pos, perl=TRUE),"Special",
                                                                          ""))))))))
                                  
  ##remove rookies without birthdates
  football_roster <- football_roster[nchar(football_roster$cs_id)>10,
                                     c("team","cs_id","player_name","draft_info","height","school","draft_year","draft_team","draft_oa","dob","pos","weight","meta_pos")]
  
  ###fix names 
  football_roster$player_name[football_roster$player_name=="Chris Harris"] <- "Chris Harris Jr" ###match PFF data
  football_roster$player_name[football_roster$player_name=="Steve Smith,Sr"] <- "Steve Smith" ###match PFF data
  
  
    ###export
  write.csv(football_roster,file="~/CWA/football_roster.csv",row.names = FALSE)
  
  library(RMySQL)
  
  conn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!', host='mysql.crowd-scout.net', db='football_all')
  on.exit(dbDisconnect(conn))
  
  dbWriteTable(conn, 'football_roster_R', as.data.frame(football_roster), overwrite=T)
