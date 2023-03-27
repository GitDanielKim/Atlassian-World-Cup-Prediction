library(readr)
library("dplyr")
library(plyr)

factor <- data.frame(matrix(nrow = length(countries), ncol = 2))
colnames(factor) <- c("Countries", "Factor")
factor$Countries <- countries

factor$Factor <- by_star$Winrate * by_covid$Winrate * by_coach$Winrate

by_goals_modified <- by_goals %>% filter(is.na(`Home Winrate`) == F | is.na(`Away Winrate`) == F)

by_goals_modified$`Home Winrate` <- mapvalues(by_goals_modified$`Home Winrate`, c(0), c(0.05))
by_goals_modified$`Away Winrate` <- mapvalues(by_goals_modified$`Away Winrate`, c(0), c(0.05))

scaled_winrates <- data.frame(matrix(nrow = nrow(by_goals_modified), ncol = 4))
colnames(scaled_winrates) <- c("Home", "Away", "Home Winrate", "Away Winrate")
scaled_winrates$Home <- by_goals_modified$Home
scaled_winrates$Away <- by_goals_modified$Away

for (i in 1:nrow(by_goals_modified)) {
  home_team = by_goals_modified[i,]$Home
  away_team = by_goals_modified[i,]$Away
  
  home_winrate = by_goals_modified[i,]$`Home Winrate`*
    factor[factor$Countries == home_team,]$Factor*
    by_home$Winrate[by_home$Countries == home_team]
  
  away_winrate = by_goals_modified[i,]$`Away Winrate`*
    factor[factor$Countries == away_team,]$Factor
  
  scaled_winrates[i,]$`Home Winrate` = home_winrate/(home_winrate + away_winrate)
  scaled_winrates[i,]$`Away Winrate` = away_winrate/(home_winrate + away_winrate)
}