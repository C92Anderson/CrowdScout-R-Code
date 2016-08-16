
###create gcode list

gcodes <- list("20002","20005")

compile.1516 <- function(gcode) {
  data <- nhlscrapr::retrieve.game(season = "20152016", gcode = gcode) [[1]]
  return(data)
}

season.201516 <-  do.call(rbind,lapply(FUN=compile.1516,gcodes))


seasons <- list("20022003","20032004")#,"20052006","20062007","20072008","20082009","20092010","20102011","20112012","20122013","20132014","20142015")

compile.0215 <- function(season) {
  data <- read.table(paste0("~/CWA/WOI Master Data/nhlscrapr-",season,".RData"),sep = "", quote = "\"'")
  return(data)
}

php.0215 <- do.call(rbind,lapply(FUN=compile.0215,seasons))


pbp.all <- rbind(
                dt <-  read.table('~/CWA/WOI Master Data/nhlscrapr-20022003.Rdata',sep = "\001"),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20032004.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20052006.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20062007.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20072008.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20082009.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20092010.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20102011.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20112012.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20122013.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20132014.Rdata'),
                  read.table('~/CWA/WOI Master Data/nhlscrapr-20142015.Rdata'))

load("~/CWA/WOI Master Data/nhlscrapr-20142015.Rdata")
load("~/CWA/WOI Master Data/garage/nhlscrapr-core.Rdata")

shots.all <- grand.data[which(grand.data$etype %in% c("SHOT","GOAL")), ]

shots.all$dist <- sqrt((shots.all$newxc - 89)**2 + (shots.all$newyc)**2)
shots.all$angle <- ifelse(shots.all$newyc == 0, 90,  ###shot straight on
                        ifelse(shots.all$newxc < 90,
                          atan((89 - shots.all$newxc) / abs(0 - shots.all$newyc)) * (180 / pi)  ,
                          0))
sample <- shots.all[1:15, c("distance","dist","subdistance","adjusted.distance","newyc","newxc","angle","etype","away.G","home.G")]
print(sample)


ggplot(data=sample, aes(x=newxc,y=newyc,color=etype,label=angle)) +
  geom_point(aes(color=etype,label=angle))

goals.only <- shots.all[which(shots.all$etype %in% c("GOAL")), 
                        c("dist","adjusted.distance","newyc","newxc","angle","away.G","home.G","period","a6","h6")]
head(goals.only[which(goals.only$period==3),],25)
head(sort(goals.only$dist, decreasing=TRUE),],25)




ggplot(data=shots.all, aes(x=xcoord,y=ycoord,color=etype)) +
    geom_point(aes(color=etype))

ggplot(data=shots.all, aes(x=newxc,y=newyc,color=etype)) +
  geom_point(aes(color=etype))

               
ggplot(data=shots.all, aes(x=xcoord,y=newxc,color=hometeam)) +
    geom_point(aes(color=hometeam))

ggplot(data=shots.all, aes(x=ycoord,y=newyc,color=hometeam)) +
  geom_point(aes(color=hometeam))

