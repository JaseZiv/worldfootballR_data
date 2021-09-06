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
  select(PlayerFBref=Player, UrlFBref=Url, UrlTmarkt=player_url, TmPos=player_position)

# some players won't have a position listed (because they haven't been matched automatically)
missing_pos <- matched_data %>% filter(!is.na(UrlTmarkt) & is.na(TmPos)) %>% pull(UrlTmarkt)

# for these URLs, we can get their positions using the `tm_player_bio` function
missing_pos_bios <- tm_player_bio(player_urls = missing_pos)

# need to clean these up from the bio data - for some reason soe of them come with the position group (say "midfield") then the true position "Left Midfielder"
# we only want "Left Midfiender"
missing_pos_bios <- missing_pos_bios %>% 
  mutate(TmPos = case_when(
    grepl(" - ", position) ~ gsub(".*- ", "", position),
    TRUE ~ position
  ))

# join the present and missing player data
matched_data <- matched_data %>%
  filter(!is.na(UrlTmarkt)) %>%
  filter(!is.na(TmPos)) %>% 
  bind_rows(
    matched_data %>% 
      filter(!is.na(UrlTmarkt)) %>%
      filter(is.na(TmPos)) %>%
      select(-TmPos) %>% 
      left_join(missing_pos_bios %>% select(URL, TmPos), by = c("UrlTmarkt" = "URL"))
  ) %>% 
  arrange(PlayerFBref)


# create final output df
final_output <- bind_rows(existing_df, matched_data) %>%
  arrange(PlayerFBref) %>% 
  distinct(UrlFBref, .keep_all=T)

# write file:
write.csv(final_output, here("raw-data", "fbref-tm-player-mapping", "output", "fbref_to_tm_mapping.csv"), row.names = FALSE)
