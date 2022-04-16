library(here)
library(worldfootballR)
all_leagues <- worldfootballR::fotmob_get_league_ids(cached = FALSE)

write.csv(
  all_leagues, 
  here::here("raw-data", "fotmob-leagues", "all_leagues.csv"), 
  row.names = FALSE
)
