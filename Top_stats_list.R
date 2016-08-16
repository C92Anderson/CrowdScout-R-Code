
###ranker function
top.player.list <- function(data, tops=0, GP.min=20) {
  
    sort.down <- c('TOI',	'GF60',	'GF.',	'GF60.RelTM',	'GF..RelTM',	'SF60',	'SF.',	'SF60.RelTM',	'SF..RelTM',	'FF60',	'FF.',	'FF60.RelTM',	'FF..RelTM',	'CF60',	'CF.',	'CF60.RelTM',	'CF..RelTM',	'iGoals',	'iAssists',	'iFirstA',	'iPoints',	'iPrimaryPoints',	'iShots',	'iFenwick',	'iCorsi',	'iSh.',	'iGoals.60',	'iAssists.60',	'iFirstA.60',	'iPoints.60',	'iPrimaryPoints.60',	'iShots.60',	'iFenwick.60',	'iCorsi.0',	'IPP',	'IGP',	'IAP',	'IPPP',	'DZFO.',	'X.ofTeam.TOI',	'X.ofTeam.GF',	'X.ofTeam.SF',	'X.ofTeam.FF',	'X.ofTeam.CF',	'X.ofTeam.DZFO')
    sort.up <- c('GA60',	'GA60.RelTM',	'SA60',	'SA60.RelTM',	'FA60',	'FA60.RelTM',	'CA60',	'CA60.RelTM')
  
    total.list <- data.frame(Player=character(),stringsAsFactors=FALSE) 
  
    data <- data[data$GP >= GP.min, ]
  
    ##sort up metrics
    sort.down.data <- data[c("Player.Name",sort.down)]
  
   for(i in c(2:ncol(sort.down.data))) {
      
     decreasing.list <- as.data.frame(head(sort.down.data[order(sort.down.data[i],decreasing=TRUE),1], tops))
     colnames(decreasing.list) <- "Player"
     decreasing.list$metric <- colnames(sort.down.data[i]) 
     total.list <- rbind(total.list, decreasing.list)
   }
  
  ##sort up metrics
    sort.up.data <- data[c("Player.Name",sort.up)]
  
  for(i in c(2:ncol(sort.up.data))) {
    
    ascending.list <- as.data.frame(head(sort.up.data[order(sort.up.data[i],decreasing=FALSE),1], tops))
    colnames(ascending.list) <- "Player"
    ascending.list$metric <- colnames(sort.up.data[i]) 
    total.list <- rbind(total.list, ascending.list)
  }
  
    player.count <- length(unique(total.list$Player))
    
    top.players <- summarize(group_by(total.list,Player), Leader.Count = n())
    top.players <- merge(top.players, season1516[,c("Player.Name","TOI")], by.x="Player",by.y="Player.Name")
    
    
  return(list(player.count, total.list,top.players))
}

season1516 <- read.csv(paste0("C:/Users/colander1/Documents/CWA/Hockey Data/Hockey Analysis/SuperSkaterStats 2015-16.csv"))

top.list <- top.player.list(season1516, 5, 50)



ggplot(data=top.list[[3]], aes(x=TOI,y=Leader.Count, label=Player)) + 
  geom_point() +
  geom_text(aes(label=Player), angle=-45, check_overlap = TRUE)
 # theme(text = element_text(size=20)) +
  labs(title=paste0("Predicted CrowdScout Score by Season\nSelect Players")) +
  labs(x="Season", y="Predicted CrowdScout Score") 
