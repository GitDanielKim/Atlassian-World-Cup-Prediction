library(readr)
library("plyr")
library("dplyr")

UEFA <- read_csv("UEFA matches/UEFA fixtures.csv")
results <- read_csv("results.csv")
coaches_unfixed <- read_csv("CoachesDay.csv")

countries_unfixed <- unique(UEFA$Home)
countries <- replace(countries_unfixed, countries_unfixed == "Bosnia-Herzegovina", "Bosnia and Herzegovina")

coaches <- coaches_unfixed %>% select(Nation, Date)

f_bycoach <- function(team, d = "#VALUE!") {
  
  if (d == "#VALUE!") {
    coach_date <- (coaches %>% filter(Nation == team))$Date
  } else {
    coach_date <- d
  }
  
  if (coach_date != "#VALUE!") {
    x <- results %>% 
      filter(date >= as.Date(coach_date) & (home_team == team|away_team == team))
    
    z1 <- x %>% filter(home_team == team)
    z2 <- x %>% filter(away_team == team)
    
    wins_home = sum(z1$home_score > z1$away_score)
    draws_home = sum(z1$home_score == z1$away_score)
    
    wins_away = sum(z2$away_score > z2$home_score)
    draws_away = sum(z2$away_score == z2$home_score)
    
    total_games = nrow(x)
    
    winrate_bycoach <- ((wins_home + wins_away) + (draws_home + draws_away)/2)/total_games
    return(winrate_bycoach)
  } else {
    winrate_bycoach <- NA
    return(winrate_bycoach)
  }
}

f_bycoach_vec <- Vectorize(f_bycoach)

by_coach <- data.frame(matrix(nrow = length(countries), ncol = 2))
colnames(by_coach) <- c("Countries", "Winrate")
by_coach$Countries <- countries

by_coach$Winrate <- f_bycoach_vec(countries)

#Modifications for Germany and Andorra / Belarus and Andorra

by_coach[which(by_coach$Countries == "Germany"), 2] = f_bycoach("Germany", "2021-07-11")
by_coach[which(by_coach$Countries == "Andorra"), 2] = f_bycoach("Andorra", "2010-02-02")

by_coach$Winrate <- mapvalues(by_coach$Winrate, c(0), c(0.05))