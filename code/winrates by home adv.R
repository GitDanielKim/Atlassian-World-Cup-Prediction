library(readr)
library("dplyr")

UEFA <- read_csv("UEFA matches/UEFA fixtures.csv")
results <- read_csv("results.csv")

results_nonneutral <- results %>% 
  filter(date >= as.Date("1974-01-01")) %>% 
  filter(neutral == FALSE) 

countries_unfixed <- unique(UEFA$Home)
countries <- replace(countries_unfixed, countries_unfixed == "Bosnia-Herzegovina", "Bosnia and Herzegovina")

f_byhome <- function(team) {
  x <- which(results_nonneutral$home_team == team)
  y <- results_nonneutral[x,]
  
  wins1 = sum(y$home_score > y$away_score)
  draws1 = sum(y$home_score == y$away_score)
  total_games1 = nrow(y)
  
  winrate_byhome <- (wins1+draws1/2)/total_games1
  
  s <- which(results_nonneutral$away_team == team)
  t <- results_nonneutral[s,]
  
  wins2 = sum(t$away_score > t$home_score)
  draws2 = sum(t$away_score == t$home_score)
  total_games2 = nrow(t)
  
  winrate_byaway <- (wins2+draws2/2)/total_games2
  
  return(winrate_byhome/winrate_byaway)
}

f_byhome_vec <- Vectorize(f_byhome)

by_home <- data.frame(matrix(nrow = length(countries), ncol = 2))
colnames(by_home) <- c("Countries", "Winrate")
by_home$Countries <- countries

by_home$Winrate <- f_byhome_vec(countries)