library(worldfootballR)
library(tidyverse)
library(googlesheets4)
library(here)
library(gt)

# first, we get updated data to ensure all new players are being captured
playing_time <- fb_big5_advanced_season_stats(season_end_year = 2023,
                                              stat_type = "playing_time",
                                              team_or_player = "player")

tm <- get_player_market_values(country_name = c("England", "Spain", "France", "Italy", "Germany"),
                               start_year = 2022)

# saveRDS(tm, here("raw-data", "fbref-tm-player-mapping", "data", "tm_data.rds"))

# read in matched data
matched_data <- read.csv("https://github.com/JaseZiv/worldfootballR_data/raw/master/raw-data/fbref-tm-player-mapping/output/fbref_to_tm_mapping.csv", stringsAsFactors = FALSE)

# create a separate df to allow for analysis of players who have changed positions
this_season <- matched_data %>%
  left_join(tm %>% select(squad, UrlTmarkt=player_url, TmPos_22_23=player_position, player_market_value_euro), by = "UrlTmarkt")

# display players that have changed data
this_season %>% 
  filter(!is.na(TmPos_22_23)) %>% 
  filter(TmPos_22_23 != TmPos) %>% 
  select(PlayerFBref, squad, TmPos, TmPos_22_23, player_market_value_euro) %>% 
  arrange(desc(player_market_value_euro)) %>% 
  mutate(player_market_value_euro = scales::dollar(player_market_value_euro, prefix = "€")) %>% 
  gt() %>% 
  tab_options(column_labels.font.size = 20,
              column_labels.font.weight = "bold")



# now overwrite older positions with the new ones
matched_data <- matched_data %>%
  left_join(tm %>% select(UrlTmarkt=player_url, TmPos_22_23=player_position), by = "UrlTmarkt") %>% 
  mutate(
    TmPos = case_when(
      is.na(TmPos_22_23) ~ TmPos,
      TRUE ~ TmPos_22_23
    )
  ) %>% 
  select(-TmPos_22_23)


#=============
# Write Files
#=============

# write file for commit to GitHub:
write.csv(matched_data, here("raw-data", "fbref-tm-player-mapping", "output", "fbref_to_tm_mapping.csv"), row.names = FALSE)

# Write file to Googlesheets:
# get the sheet id
ss <- as_sheets_id("https://docs.google.com/spreadsheets/d/1GjjS9IRp6FVzVX5QyfmttMk8eYBtIzuZ_YIM0VWg8OY/edit#gid=61874932") %>% 
  as.character()

# write the sheet
sheet_write(matched_data,
            ss,
            sheet = "fbref_to_tm_mapping")


