library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(twitteR);library(httr); library(data.table); library(reshape2);
library(shiny);library(xts);library(DBI)

txt <- element_text(size = 18, colour = "grey25", face = "plain")
bold_txt <- element_text(size = 20, colour = "navy", face = "bold")

theme_standard <- function(base_size = 16, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      strip.background = element_blank(), 
      
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line( colour = "white", size = 2), 
      
      panel.background = element_rect(fill="grey90"),
      plot.background = element_rect(fill="grey90"),
      legend.background = element_rect(fill="grey90"),
      legend.key = element_rect(fill="grey90", size = 20),
      legend.key.size = unit(1,"cm"),
      
      panel.border = element_blank(), 
      
      line = element_line( colour = "white", size = 2),
      axis.text.x = element_text(angle = 90, hjust = 1),
      text = txt, 
      plot.title = bold_txt, 
      
      axis.title = txt, 
      axis.text = txt, 
      
      legend.title = bold_txt, 
      legend.text = txt ) 
}
