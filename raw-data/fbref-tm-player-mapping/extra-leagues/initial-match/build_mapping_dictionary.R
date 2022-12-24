library(worldfootballR)
library(tidyverse)
library(here)

fbref <- readRDS(here("raw-data", "fbref-tm-player-mapping", "extra-leagues", "initial-match", "fbref_extra_leagues.rds"))
tm1 <- readRDS(here("raw-data", "fbref-tm-player-mapping", "extra-leagues", "initial-match", "tm_players_extra_tier1.rds"))
tm2 <- readRDS(here("raw-data", "fbref-tm-player-mapping", "extra-leagues", "initial-match", "tm_players_championship.rds"))

tm <- bind_rows(tm1,tm2)

matched_data <- read.csv("https://github.com/JaseZiv/worldfootballR_data/raw/master/raw-data/fbref-tm-player-mapping/output/fbref_to_tm_mapping.csv", stringsAsFactors = FALSE)


fbref <- fbref %>% filter(!Url %in% matched_data$UrlFBref)


fbref <- fbref %>%
  select(season_end_year, Squad, competition_name, Player, Nation, Born, Url) %>%
  mutate(fbref_surname = str_squish(gsub(".*\\s", "", Player))) %>%
  arrange(Player, Url, desc(season_end_year)) %>%
  distinct(Url, .keep_all = T)

tm <- tm %>%
  select(comp_name, region, country, season_start_year, squad, player_name, player_position,
         player_dob, player_nationality, player_market_value_euro, player_url) %>%
  arrange(player_name)

# want a df to help with inspection of names with special characters
tm_unique <- tm %>%
  arrange(player_name, player_url, desc(season_start_year)) %>%
  distinct(player_name, player_dob, player_url, .keep_all = T) %>%
  mutate(tm_surname = str_squish(gsub(".*\\s", "", player_name)),
         tm_yob = as.character(lubridate::year(player_dob)))

#----- primary join type: -----#
# here I will join the two datasets on the player names
# joined_primary <- fbref %>% select(Player, Born, Url) %>% distinct(Url, .keep_all = T) %>%
#   left_join(tm %>% select(player_name, player_dob, player_url) %>% distinct(player_url, .keep_all = T), by = c("Player" = "player_name"))

joined_primary <- fbref %>% select(Player, Born, Url, Squad, Nation) %>% distinct(Url, .keep_all = T) %>%
  left_join(tm_unique %>% select(player_name, player_dob, player_url, tm_yob, tm_squad=squad, tm_nationality=player_nationality, player_position) %>% distinct(player_url, .keep_all = T),
            by = c("Player" = "player_name", "Born" = "tm_yob"))

# arrange by player name
joined_primary <- joined_primary %>% arrange(Player)


# these players have multiple records in each data set - think "Adama Traoré" or "Rafael" or "Raúl García" for example
# will need to manually go through each of these to map the correct player
duplicate_players <- joined_primary %>% count(Player, Url, sort = T) %>% filter(n > 1) %>% pull(Url)
duplicate_players <- joined_primary %>% filter(Url %in% duplicate_players)

# # inspecting these records, I might be able to get some more hits when comparing the player's YOB
# no_longer_dups <- duplicate_players %>%
#   mutate(tm_yob = lubridate::year(player_dob)) %>%
#   filter(Born == tm_yob)
#
# still_dups <- no_longer_dups %>%
#   count(Player, Url, Born) %>% filter(n>1) %>% pull(Url) %>% unique()
#
# still_dups <- duplicate_players %>%
#   filter(Url %in% still_dups)
#
# no_longer_dups <- no_longer_dups %>%
#   filter(!Url %in% still_dups$Url)

# now remove these records from the raw joined data
# IMPORTANT: remember to add `duplicate_players_df` that has been cleaned manually back to the main df
joined_primary <- joined_primary %>%
  filter(!Url %in% duplicate_players$Url)

# get a full list of joins on full player name that I'm happy with
joined_complete <- joined_primary %>%
  filter(!is.na(player_url))

# get a list of records where there were no matches on full player name
joined_missing <- joined_primary %>%
  filter(is.na(player_url))

#----- secondary join type: -----#
# here I'll try to join on surname and year of birth - would be nice to use DOB instead but I don't have it for FBref players
joined_secondary <- joined_missing %>% select(-player_position) %>% 
  mutate(fbref_surname = gsub(".*\\s", "", Player)) %>%
  select(-player_dob, -player_url) %>%
  left_join(tm_unique, by = c("fbref_surname" = "tm_surname", "Born" = "tm_yob"))

# now there are some more duplicates as a result of this secondary join method
additional_duplicated_players <- joined_secondary %>%
  filter(!is.na(player_url)) %>%
  count(Player, Url, sort = T) %>%
  filter(n > 1) %>% pull(Url) %>% unique()

additional_duplicated_players <- joined_secondary %>%
  filter(Url %in% additional_duplicated_players)


# combine all duplicated joins for manual rework:
duplicate_players <- duplicate_players %>%
  bind_rows(additional_duplicated_players)

duplicate_players <- duplicate_players %>%
  select(-fbref_surname, -player_name)


joined_secondary <- joined_secondary %>%
  filter(!is.na(player_url),
         !Url %in% additional_duplicated_players$Url) %>%
  select(Player, Born, Url, player_dob, player_url, player_position)


joined_finished <- joined_complete %>%
  filter(!is.na(player_url)) %>%
  bind_rows(joined_secondary)

joined_finished <- joined_finished %>%
  select(-Squad, -Nation, -tm_squad, -tm_nationality)


# create a file for manual rework by removing any of the records that have been matched since the creation of `joined_missing`:
joined_missing <- joined_missing %>%
  filter(!Url %in% joined_finished$Url,
         !Url %in% duplicate_players$Url)


# write files to work on manually
write.csv(joined_finished, here("raw-data", "fbref-tm-player-mapping", "extra-leagues", "initial-match", "joined_finished.csv"), row.names = F)
write.csv(joined_missing, here("output", "initial-match", "working-files", "joined_missing.csv"), row.names = F)
write.csv(tm_unique, here("output", "initial-match", "working-files", "tm_unique.csv"), row.names = F)
write.csv(duplicate_players, here("raw-data", "fbref-tm-player-mapping", "extra-leagues", "initial-match", "working-files", "duplicate_players_df.csv"), row.names = F)
