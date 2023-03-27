library(readr)
library("dplyr")

UEFA <- read_csv("UEFA matches/UEFA fixtures.csv")
UEFA$Home <- replace(UEFA$Home, UEFA$Home == "Bosnia-Herzegovina", "Bosnia and Herzegovina")
UEFA$Away <- replace(UEFA$Away, UEFA$Away == "Bosnia-Herzegovina", "Bosnia and Herzegovina")

results <- read_csv("results.csv")

f_bygoals <- function(home, away) {
  x <- results %>% filter(date >= as.Date("1974-01-01")) %>% 
    filter_at(vars(home_team, away_team), all_vars(. %in% c(home, away)))
  
  homescore = 0
  awayscore = 0
  
  if (nrow(x) == 0) {
    homescore = 0
    awayscore = 0
  } else {
      for (i in 1:nrow(x)) {
        if (x$home_team[i] == home) {
          homescore = homescore + x$home_score[i]
          awayscore = awayscore + x$away_score[i]
        } else {
          homescore = homescore + x$away_score[i]
          awayscore = awayscore + x$home_score[i]
        }
      }
  }
    
  total = homescore + awayscore
  
  homepercent = homescore/total
  awaypercent = awayscore/total
  
  return(c(homepercent, awaypercent))
}


by_goals <- data.frame(matrix(nrow = nrow(UEFA), ncol = 4))
colnames(by_goals) <- c("Home", "Away", "Home Winrate", "Away Winrate")
by_goals[,1] <- UEFA$Home
by_goals[,2] <- UEFA$Away

for (i in 1:nrow(UEFA)) {
  home = UEFA[i,]$Home
  away = UEFA[i,]$Away
  
  by_goals[i, 3] = f_bygoals(home, away)[1]
  by_goals[i, 4] = f_bygoals(home, away)[2]
}