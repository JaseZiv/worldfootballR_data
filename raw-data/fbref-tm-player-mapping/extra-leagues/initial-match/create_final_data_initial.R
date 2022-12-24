library(worldfootballR)
library(tidyverse)
library(googlesheets4)
library(here)

existing_df <- read.csv("https://github.com/JaseZiv/worldfootballR_data/raw/master/raw-data/fbref-tm-player-mapping/output/fbref_to_tm_mapping.csv", stringsAsFactors = FALSE)

# read in files
joined_finished <- read.csv(file.path("joined_finished.csv"), stringsAsFactors = F)
joined_missing <- read.csv(file.path("working-files", "joined_missing.csv"), stringsAsFactors = F)
duplicate_players <- tryCatch(read.csv(file.path("working-files", "duplicate_players_df_manual_fix.csv"), stringsAsFactors = F), 
                              error = function(e) data.frame())

duplicate_players <- duplicate_players %>% select(Player, Born, Url, player_dob, player_url, player_position)


matched_data <- bind_rows(joined_finished, joined_missing, duplicate_players) %>%
  arrange(Player) %>%
  mutate(player_url = ifelse(player_url == "", NA_character_, player_url))


matched_data <- matched_data %>%
  select(PlayerFBref=Player, UrlFBref=Url, UrlTmarkt=player_url, TmPos=player_position)

# some players won't have a position listed (because they haven't been matched automatically)
missing_pos <- matched_data %>% filter(!is.na(UrlTmarkt) & is.na(TmPos)) %>% pull(UrlTmarkt)

# for these URLs, we can get their positions using the `tm_player_bio` function

missing_pos_bios <- data.frame()

for (i in 1:length(missing_pos)) {
  print(paste0("scraping ", i, "of", length(missing_pos)))
  df <- tryCatch(tm_player_bio(player_urls = missing_pos[i]), error = function(e) data.frame())
  missing_pos_bios <- bind_rows(missing_pos_bios, df)
}


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

#=============
# Write Files
#=============

# write file for commit to GitHub:
write.csv(final_output, here("raw-data", "fbref-tm-player-mapping", "output", "fbref_to_tm_mapping.csv"), row.names = FALSE)

# Write file to Googlesheets:
# get the sheet id
ss <- as_sheets_id("https://docs.google.com/spreadsheets/d/1GjjS9IRp6FVzVX5QyfmttMk8eYBtIzuZ_YIM0VWg8OY/edit#gid=61874932") %>%
  as.character()

# write the sheet
sheet_write(final_output,
            ss,
            sheet = "fbref_to_tm_mapping")
