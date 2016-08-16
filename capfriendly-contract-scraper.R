

all.contracts <- read.csv('~/CWA/Hockey Data/capfriendly-allcontracts.csv')


jj <- subset(all.contracts, PLAYER %in% "Jaromir Jagr")

library(dplyr)
library(RCurl)
library(rvest)
library(foreach)

url <- read_html("https://www.capfriendly.com/signings")

contract.list <- url %>% html_nodes("td") %>% html_text() %>% matrix(byrow=TRUE, ncol=9) %>% data.frame()


contracts.capfriendly <- html_nodes(url, "td")
contracts.capfriendly <- html_text(contracts.capfriendly)
roster3 <- roster2[toupper(roster2) != c("RESERVES","ACTIVE PLAYERS")]
roster4 <- matrix(roster3[c(2:length(roster3))], byrow = TRUE, ncol = 11) %>% data.frame()
