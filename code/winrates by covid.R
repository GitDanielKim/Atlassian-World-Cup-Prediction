library(readr)
library("dplyr")

UEFA <- read_csv("UEFA matches/UEFA fixtures.csv")

results <- read_csv("results.csv")
results_covid <- results %>% filter(date >= as.Date("2020-01-21"))

countries_unfixed <- unique(UEFA$Home)
countries <- replace(countries_unfixed, countries_unfixed == "Bosnia-Herzegovina", "Bosnia and Herzegovina")

f_bycovid <- function(team) {
  x <- which(results_covid$home_team == team | results_covid$away_team == team)
  y <- results_covid[x,]
  
  z1 <- y %>% filter(home_team == team)
  z2 <- y %>% filter(away_team == team)
  
  wins_home = sum(z1$home_score > z1$away_score)
  draws_home = sum(z1$home_score == z1$away_score)
  
  wins_away = sum(z2$away_score > z2$home_score)
  draws_away = sum(z2$away_score == z2$home_score)
  
  total_games = nrow(y)
  
  winrate_bycovid <- ((wins_home + wins_away) + (draws_home + draws_away)/2)/total_games
  return(winrate_bycovid)
}

f_bycovid_vec <- Vectorize(f_bycovid)

by_covid <- data.frame(matrix(nrow = length(countries), ncol = 2))
colnames(by_covid) <- c("Countries", "Winrate")
by_covid$Countries <- countries

by_covid$Winrate <- f_bycovid_vec(countries)