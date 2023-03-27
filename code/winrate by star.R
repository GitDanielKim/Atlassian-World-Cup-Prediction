library(readr)
library("dplyr")

results <- read_csv("results.csv")
UEFA <- read_csv("UEFA matches/UEFA fixtures.csv")
players <- read_csv("players_21.csv")

countries_unfixed <- unique(UEFA$Home)
countries <- replace(countries_unfixed, countries_unfixed == "Bosnia-Herzegovina", "Bosnia and Herzegovina")

players_short <- players %>% filter(overall >= 83) %>% select(long_name, age, nationality)

threshold <- 18

f_bystar <- function(team) {
  x <- players_short %>% filter(nationality == "Argentina")
  age_diff <- min(x$age) - threshold
  youngest <- as.Date(paste0(2021-age_diff, "-01", "-01"))
  
  y <- results %>% 
    filter(date >= youngest & (home_team == team | away_team == team))
  
  z1 <- y %>% filter(home_team == team)
  z2 <- y %>% filter(away_team == team)
  
  wins_home = sum(z1$home_score > z1$away_score)
  draws_home = sum(z1$home_score == z1$away_score)
  
  wins_away = sum(z2$away_score > z2$home_score)
  draws_away = sum(z2$away_score == z2$home_score)
  
  total_games = nrow(y)
  
  winrate_bystar <- ((wins_home + wins_away) + (draws_home + draws_away)/2)/total_games
  return(winrate_bystar)
}

f_bystar_vec <- Vectorize(f_bystar)

by_star <- data.frame(matrix(nrow = length(countries), ncol = 2))
colnames(by_star) <- c("Countries", "Winrate")
by_star$Countries <- countries

by_star$Winrate <- f_bystar_vec(countries)