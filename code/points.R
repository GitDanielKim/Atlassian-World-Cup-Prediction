points <- data.frame(matrix(nrow = length(countries), ncol = 2))
colnames(points) <- c("Countries", "Points")
points$Countries <- countries

for (i in 1:nrow(points)) {
  
  team = points$Countries[i]
  
  x <- scaled_winrates %>% 
    filter(Home == team | Away == team)
  
  z1 <- x %>% filter(Home == team)
  z2 <- x %>% filter(Away == team)
  
  matchpoint = 0
  
  if (nrow(z1) != 0) {
    for (n in 1:nrow(z1)) {
      if (z1$`Home Winrate`[n] > 0.55) {
        matchpoint = matchpoint + 3
      } else if (z1$`Home Winrate`[n] <= 0.55 & z1$`Home Winrate`[n] >= 0.45) {
        matchpoint = matchpoint + 1
      } else {
        matchpoint = matchpoint
      }
    }
  } else {
    matchpoint = matchpoint
  }
  
  if (nrow(z2) != 0) {
    for (n in 1:nrow(z2)) {
      if (z2$`Away Winrate`[n] > 0.55) {
        matchpoint = matchpoint + 3
      } else if (z2$`Away Winrate`[n] <= 0.55 & z2$`Away Winrate`[n] >= 0.45) {
        matchpoint = matchpoint + 1
      } else {
        matchpoint = matchpoint
      }
    }
  } else {
    matchpoint = matchpoint
  }
  
  points[i,]$Points = matchpoint
}

write.csv(points, "C:\\Users\\Jay\\OneDrive\\University Extra (Case comps, etc.)\\Atlassian Case Comp\\Results\\UEFA_points3.csv", row.names = F)