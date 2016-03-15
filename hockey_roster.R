##install.packages("rvest")
##install.packages("dplyr")
##install.packages("RCurl")
##install.packages("foreach")

library(dplyr)
library(RCurl)
library(rvest)
library(foreach)

hockey_roster <- data.frame(no=character(),
                         player_name=character(),
                         pos=character(),
                         height=character(),
                         weight=integer(),
                         dob=character(),
                         age=integer(),
                         pob=character(),
                         team=character(), 
                              stringsAsFactors=TRUE) 

teams <- c("Ducks", "Coyotes", "Bruins", "Sabres", "Hurricanes", "BlueJackets",
           "Flames", "Blackhawks", "Avalanche", "Stars", "RedWings", "Oilers",
           "Panthers", "Kings", "Wild", "Canadiens", "Devils", "Predators",
           "Islanders", "Rangers", "Senators", "Flyers", "Penguins", "Sharks",
           "Blues", "Lightning", "MapleLeafs", "Canucks", "Jets", "Capitals")

mo_list <- data.frame(matrix(c('Jan','1',
            'Feb','2',
            'Mar','3',
            'Apr','4',
            'May','5',
            'Jun','6',
            'Jul','7',
            'Aug','8',
            'Sep','9',
            'Oct','10',
            'Nov','11',
            'Dec','12'),byrow = TRUE, ncol = 2))
colnames(mo_list) <- c("nm","no")
            

for(i in teams){  
  
url <- read_html(print(paste0("http://",i,".nhl.com/club/roster.htm")))

####FORWARDS
fwd <- html_nodes(url, ".data:nth-child(2) td")

fwd1 <- matrix(html_text(fwd)[c(9:length(fwd))], byrow = TRUE, ncol = 8) %>% data.frame()

colnames(fwd1) <- c("no","player_name","pos","height","weight","dob","age","pob")

####DEFENSEMEN

def <- html_nodes(url,".data:nth-child(4) .rwOdd td , .data:nth-child(4) .rwEven td")

def1 <- matrix(html_text(def)[c(1:length(def))], byrow = TRUE, ncol = 7) %>% data.frame()

colnames(def1) <- c("no","player_name","height","weight","dob","age","pob")
def1$pos <- "D"

####GOALIES
gs <- html_nodes(url,".data:nth-child(6) .rwEven td+ td , .data:nth-child(6) .rwOdd td , .data:nth-child(6) .rwEven .noLftBdr")

####round down to multiple of 7
gs1 <- matrix(html_text(gs)[c(1:(floor(length(gs)/7)*7))], byrow = TRUE, ncol = 7) %>% data.frame()

colnames(gs1) <- c("no","player_name","height","weight","dob","age","pob")
gs1$pos <- "G"

roster <- rbind(fwd1,def1,gs1)

roster$team <- print(paste(i))

hockey_roster <- rbind(hockey_roster, roster)

}

##clean player name
hockey_roster$player_name <- ifelse(regexpr('Â',hockey_roster$player_name)>0, 
                             substr(hockey_roster$player_name,1,regexpr('Â',hockey_roster$player_name)-1),
                             hockey_roster$player_name)
hockey_roster$player_name <- ifelse(regexpr('"',hockey_roster$player_name)>0, 
                             substr(hockey_roster$player_name,1,regexpr('"',hockey_roster$player_name)-1),
                             hockey_roster$player_name)
##remove newline, tab from name
hockey_roster$player_name <- gsub("[\t]","",hockey_roster$player_name)
hockey_roster$player_name <- gsub("[\n]","",hockey_roster$player_name)
hockey_roster$player_name <- gsub("'","",hockey_roster$player_name)

##remove newline, tab from jersey
hockey_roster$no <- gsub("[\t]","",hockey_roster$no)
hockey_roster$no <- gsub("[\n]","",hockey_roster$no)
hockey_roster$height <- gsub('"',"",hockey_roster$height)

###set brithdate
hockey_roster$dob <- as.Date(ifelse(is.na(match(substr(hockey_roster$dob,1,3),mo_list$nm)),
                                    as.Date(hockey_roster$dob, format='%d %b %Y'),
                                    as.Date(hockey_roster$dob, format='%b %d, %Y'))
                           , origin="1970-01-01")

##create player name xwalk
hockey_roster$pnm <- toupper(gsub(" ","",hockey_roster$player_name))
hockey_roster$pnm <- gsub("[^A-Za-z]", "", hockey_roster$pnm)

# Fix duplicate names / Réparer noms doubles
hockey_roster$pnm[which(hockey_roster$pnm == "ERIKKARLSSON" & hockey_roster$team == "Hurricanes")] <- "ERIKKARLSSON2"
hockey_roster$pnm[which(hockey_roster$pnm == "ERIKGUSTAFSSON" & hockey_roster$team == "Flyers")] <- "ERIKGUSTAFSSON2"
hockey_roster$pnm[which(hockey_roster$pnm == "PKSUBBAN" | hockey_roster$pnm == "PKSUBBAN")] <- "PKSUBBAN"
hockey_roster$pnm[which(hockey_roster$pnm == "TJOSHIE" | hockey_roster$pnm == "TJOSHIE")] <- "TJOSHIE"
hockey_roster$pnm[which(hockey_roster$pnm == "BJCROMBEEN" | hockey_roster$pnm == "BJCROMBEEN" | hockey_roster$pnm == "BRANDONCROMBEEN")] <- "BJCROMBEEN"
hockey_roster$pnm[which(hockey_roster$pnm == "ILJABRYZGALOV")] <- "ILYABRYZGALOV"
hockey_roster$pnm[which(hockey_roster$pnm == "CAMERONBARKER")] <- "CAMBARKER"
hockey_roster$pnm[which(hockey_roster$pnm == "CHRISVANDE VELDE")] <- "CHRISVANDEVELDE"
hockey_roster$pnm[which(hockey_roster$pnm == "DANIELCARCILLO")] <- "DANCARCILLO"
hockey_roster$pnm[which(hockey_roster$pnm == "DANIELCLEARY")] <- "DANCLEARY"
hockey_roster$pnm[which(hockey_roster$pnm == "DAVID JOHNNYODUYA")] <- "JOHNNYODUYA"
hockey_roster$pnm[which(hockey_roster$pnm == "DAVIDBOLLAND")] <- "DAVEBOLLAND"
hockey_roster$pnm[which(hockey_roster$pnm == "DWAYNEKING")] <- "DJKING"
hockey_roster$pnm[which(hockey_roster$pnm == "EVGENIIDADONOV")] <- "EVGENYDADONOV"
hockey_roster$pnm[which(hockey_roster$pnm == "FREDDYMODIN")] <- "FREDRIKMODIN"
hockey_roster$pnm[which(hockey_roster$pnm == "HARRISONZOLNIERCZYK")] <- "HARRYZOLNIERCZYK"
hockey_roster$pnm[which(hockey_roster$pnm == "J PDUMONT" | hockey_roster$pnm == "JEANPIERREDUMONT")] <- "JPDUMONT"
hockey_roster$pnm[which(hockey_roster$pnm == "JEANFRANCOISJACQUES")] <- "JFJACQUES"
hockey_roster$pnm[which(hockey_roster$pnm == "JONATHANAUDYMARCHESSAULT")] <- "JONATHANMARCHESSAULT"
hockey_roster$pnm[which(hockey_roster$pnm == "JOSHUAHENNESSY")] <- "JOSHHENNESSY"
hockey_roster$pnm[which(hockey_roster$pnm == "KRISTOPHERLETANG")] <- "KRISLETANG"
hockey_roster$pnm[which(hockey_roster$pnm == "KRYSTOFERBARCH")] <- "KRYSBARCH"
hockey_roster$pnm[which(hockey_roster$pnm == "MARTINST LOUIS")] <- "MARTINST LOUIS"
hockey_roster$pnm[which(hockey_roster$pnm == "MATTHEWCARLE")] <- "MATTCARLE"
hockey_roster$pnm[which(hockey_roster$pnm == "MATTHEWDUMBA")] <- "MATTDUMBA"
hockey_roster$pnm[which(hockey_roster$pnm == "JOSEPHCORVO")] <- "JOECORVO"
hockey_roster$pnm[which(hockey_roster$pnm == "TOBYENSTROM")] <- "TOBIASENSTROM"
hockey_roster$pnm[which(hockey_roster$pnm == "MICHAELSANTORELLI")] <- "MIKESANTORELLI"
hockey_roster$pnm[which(hockey_roster$pnm == "PIERREPARENTEAU" | hockey_roster$pnm == "PIERREALEXANDREPARENTEAU")] <- "PAPARENTEAU"
hockey_roster$pnm <- gsub("ALEXANDER|ALEXANDRE", "ALEX", hockey_roster$pnm)
hockey_roster$pnm <- gsub("CHRISTOPHER", "CHRIS", hockey_roster$pnm)

###team abbreviations
team_xwalk <- data.frame(matrix(c('Ducks','ANA',
                       'Coyotes','ARI',
                       'Bruins','BOS',
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
                       'Wild','MIN',
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

colnames(team_xwalk) <- c("team","tm")

hockey_roster <- merge(hockey_roster, team_xwalk, by="team")

###remove retired players
non_nhl_players <- data.frame(matrix(c("MATTIASOHLUND","NATHANHORTON","CHRISPRONGER","JESSEWINCHESTER","ERICBOULTON"),ncol = 1))
colnames(non_nhl_players) <- "player_name"

hockey_roster <- hockey_roster[-match(hockey_roster$pnm,non_nhl_players$player_name,nomatch=1),]

####merge on NHL_ID
nhlid_xwalk = read.csv("C:/Users/colander1/Documents/CWA/playerid_xwalk.csv")

hockey_roster <- merge(hockey_roster,nhlid_xwalk[,1:2], by="pnm", all.x = TRUE)

###export
write.csv(hockey_roster,file="~/CWA/hockey_roster.csv",row.names = FALSE)

###export names without IDs
no_nhlid <- hockey_roster[is.na(hockey_roster$nhl_id), ]

write.csv(no_nhlid,file="~/CWA/hockey_noid.csv")
print(no_nhlid)