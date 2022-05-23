library(worldfootballR)
library(dplyr)
library(stringr)
library(here)


seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)


latest_season <- seasons %>%
  # filtering out things that aren't domestic leagues:
  dplyr::filter(stringr::str_detect(.data$competition_type, "Big 5 European Leagues")) %>% 
  group_by(country) %>% slice_max(season_end_year) %>% 
  pull(season_end_year)



stat_types <- c("standard", "playing_time", "shooting", "passing", "passing_types", "gca", 
               "defense", "possession", "misc", "keepers", "keepers_adv")


#==========================================================================================
# Update Player Advanced Stats --------------------------------------------
#==========================================================================================

for(each_stat in stat_types) {
  
  print(paste0("Updating player stat: ", each_stat))
  
  df <- readRDS(here("data", "fb_big5_advanced_season_stats", paste0("big5_player_", each_stat, ".rds")))
  
  new_dat <- fb_big5_advanced_season_stats(season_end_year= latest_season, stat_type= each_stat, team_or_player= "player")
  
  df <- df %>% 
    filter(Season_End_Year != latest_season)
  
  df <- bind_rows(df, new_dat)
  
  saveRDS(df, here("data", "fb_big5_advanced_season_stats", paste0("big5_player_", each_stat, ".rds")))
}



#==========================================================================================
# Update Team Advanced Stats ----------------------------------------------
#==========================================================================================

for(each_stat in stat_types) {
  
  print(paste0("Updating team stat: ", each_stat))
  
  df <- readRDS(here("data", "fb_big5_advanced_season_stats", paste0("big5_team_", each_stat, ".rds")))
  
  new_dat <- fb_big5_advanced_season_stats(season_end_year= latest_season, stat_type= each_stat, team_or_player= "team")
  
  df <- df %>% 
    filter(Season_End_Year != latest_season)
  
  df <- bind_rows(df, new_dat)
  
  saveRDS(df, here("data", "fb_big5_advanced_season_stats", paste0("big5_team_", each_stat, ".rds")))
}

scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
saveRDS(scrape_time_utc, here("data", "fb_big5_advanced_season_stats", "scrape_time_big5_advanced_season_stats.rds"))
