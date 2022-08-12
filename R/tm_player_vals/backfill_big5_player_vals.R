library(worldfootballR)
library(tidyverse)
library(here)


for(each_season in c(2010:2022)) {
  print(paste0("scraping season: ", each_season))
  
  each_df <- tm_player_market_values(country_name = c("England", "Spain", "France", "Italy", "Germany"),
                                      start_year = each_season)
  df <- bind_rows(df, each_df)
}


saveRDS(full, here("data", "tm_player_vals", "big5_player_vals.rds"))




