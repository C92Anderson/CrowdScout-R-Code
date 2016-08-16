##install.packages("rvest")
##install.packages("dplyr")
##install.packages("RCurl")
##install.packages("foreach")
#install.packages("xlsx")
#devtools::install_github("hadley/readxl")

library(dplyr)
library(RCurl)
library(rvest)
library(foreach)
library(readxl)
library(RMySQL)

###not working
#pff.url <- read_html("https://grades.profootballfocus.com/#/ratings/positions/show/QB")
#pff.url %>% html_nodes(".text-center.ng-binding , .text-nowrap , .text-right , .table__column-small , .tablesort-sortable") %>% html_text()

excel2mysql <- function(pos) {
  
  data <- read_excel("C:/Users/colander1/Documents/CWA/PFFRatings.xlsx",sheet=pos) 
  
  #####insert into phpmysql
  library(RMySQL)
  conn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!',host='mysql.crowdscoutsports.com', db='football_all')
  on.exit(dbDisconnect(conn))
  
  #fix names
  data$NAME <- gsub("[.]","",data$NAME)
  data$NAME <- gsub("[']","",data$NAME)
  data$NAME <- gsub("\\(.+?\\)","",data$NAME)
  data$NAME <-  gsub("\\b[a-zA-Z0-9]{1}\\b","", data$NAME) 
  data$NAME <-  gsub("  "," ", data$NAME) 
  data$meta_pos <- substr(pos,5,nchar(pos))
  data$meta_pos[data$meta_pos=="Backers" | data$meta_pos=="Linemen"] <- "Front7" 
  
  dbWriteTable(conn, pos, as.data.frame(data), overwrite=T)
 
}

excel2mysql('PFF_Passers')
excel2mysql('PFF_Runners')
excel2mysql('PFF_Receivers')
excel2mysql('PFF_Blockers')
excel2mysql('PFF_Secondary')
excel2mysql('PFF_Backers')
excel2mysql('PFF_Linemen')
