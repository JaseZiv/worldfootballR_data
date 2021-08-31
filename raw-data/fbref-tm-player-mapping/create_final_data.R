library(worldfootballR)
library(tidyverse)
library(here)

existing_df <- read.csv("https://github.com/JaseZiv/worldfootballR_data/raw/master/raw-data/fbref-tm-player-mapping/output/fbref_to_tm_mapping.csv", stringsAsFactors = FALSE)

# read in files
joined_finished <- read.csv(here("raw-data", "fbref-tm-player-mapping", "output", "working-files", "joined_finished.csv"), stringsAsFactors = F)
joined_missing <- read.csv(here("raw-data", "fbref-tm-player-mapping", "output", "working-files", "joined_missing_manual_fix.csv"), stringsAsFactors = F)
duplicate_players <- tryCatch(read.csv(here("raw-data", "fbref-tm-player-mapping", "output", "working-files", "duplicate_players_df_manual_fix.csv"), stringsAsFactors = F) %>%
  select(-fbref_surname, -player_name), error = function(e) data.frame())


matched_data <- bind_rows(joined_finished, joined_missing, duplicate_players) %>%
  arrange(Player) %>%
  mutate(player_url = ifelse(player_url == "", NA_character_, player_url))


matched_data <- matched_data %>%
  select(PlayerFBref=Player, UrlFBref=Url, UrlTmarkt=player_url)

final_output <- bind_rows(existing_df, matched_data) %>%
  arrange(PlayerFBref) %>% 
  distinct(UrlFBref, .keep_all=T)

write.csv(final_output, here("raw-data", "fbref-tm-player-mapping", "output", "fbref_to_tm_mapping.csv"), row.names = FALSE)
