library(worldfootballR)
library(tidyverse)
library(here)

source("R/piggyback.R")

seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)


latest_season <- seasons %>%
  # filtering out things that aren't domestic leagues:
  dplyr::filter(stringr::str_detect(.data$competition_type, "Big 5 European Leagues")) %>% 
  group_by(country) %>% slice_max(season_end_year) %>% 
  pull(season_end_year)

#================================================================================================
# Player Advanced Stats ---------------------------------------------------
#================================================================================================

backfill_player_advanced <- function(season_end, stat) {
  df <- fb_big5_advanced_season_stats(season_end_year= season_end, stat_type= stat, team_or_player= "player")
  df <- df %>% relocate(Url, .after = last_col())
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  attr(df, "scrape_timestamp") <- scrape_time_utc
  
  # saveRDS(df, here("data", "fb_big5_advanced_season_stats", paste0("big5_player_", stat, ".rds")))
  write_worldfootballr(x=df, name = paste0("big5_player_", stat), tag = "fb_big5_advanced_season_stats", ext = "rds")
  
}


backfill_player_advanced(season_end= c(2010:2023), stat= "standard")
backfill_player_advanced(season_end= c(2010:2023), stat= "playing_time")
backfill_player_advanced(season_end= c(2010:2023), stat= "shooting")
backfill_player_advanced(season_end= c(2018:2023), stat= "passing")
backfill_player_advanced(season_end= c(2018:2023), stat= "passing_types")
backfill_player_advanced(season_end= c(2018:2023), stat= "gca")
backfill_player_advanced(season_end= c(2018:2023), stat= "defense")
backfill_player_advanced(season_end= c(2018:2023), stat= "possession")
backfill_player_advanced(season_end= c(2010:2023), stat= "misc")
backfill_player_advanced(season_end= c(2010:2023), stat= "keepers")
backfill_player_advanced(season_end= c(2018:2023), stat= "keepers_adv")


#================================================================================================
# Team Advanced Stats -----------------------------------------------------
#================================================================================================

backfill_team_advanced <- function(season_end, stat) {
  df <- fb_big5_advanced_season_stats(season_end_year= season_end, stat_type= stat, team_or_player= "team")
  df <- df %>% relocate(Url, .after = last_col())
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  attr(df, "scrape_timestamp") <- scrape_time_utc
  
  # saveRDS(df, here("data", "fb_big5_advanced_season_stats", paste0("big5_team_", stat, ".rds")))
  write_worldfootballr(x=df, name = paste0("big5_team_", stat), tag = "fb_big5_advanced_season_stats", ext = "rds")
  
}


backfill_team_advanced(season_end= c(2010:2023), stat= "playing_time")

backfill_team_advanced(season_end= c(2010:2023), stat= "standard")
backfill_team_advanced(season_end= c(2010:2023), stat= "shooting")
backfill_team_advanced(season_end= c(2018:2023), stat= "passing")
backfill_team_advanced(season_end= c(2018:2023), stat= "passing_types")
backfill_team_advanced(season_end= c(2018:2023), stat= "gca")
backfill_team_advanced(season_end= c(2018:2023), stat= "defense")
backfill_team_advanced(season_end= c(2018:2023), stat= "possession")
backfill_team_advanced(season_end= c(2010:2023), stat= "misc")
backfill_team_advanced(season_end= c(2010:2023), stat= "keepers")
backfill_team_advanced(season_end= c(2018:2023), stat= "keepers_adv")


