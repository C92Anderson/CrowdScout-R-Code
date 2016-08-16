

setwd("C:/Users/colander1/Documents/CWA/Hockey Data")

load("roster.Rda")

head(roster$piclink)

player_list <- list(unique(roster$Full.Name,roster$pick))


load("pbp20152016.Rda")