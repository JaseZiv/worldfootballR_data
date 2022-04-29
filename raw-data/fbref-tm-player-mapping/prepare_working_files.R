library(worldfootballR)
library(tidyverse)
library(here)

playing_time <- fb_big5_advanced_season_stats(season_end_year = 2022,
                                              stat_type = "playing_time",
                                              team_or_player = "player")

tm <- get_player_market_values(country_name = c("England", "Spain", "France", "Italy", "Germany"),
                                       start_year = 2021)


matched_data <- read.csv("https://github.com/JaseZiv/worldfootballR_data/raw/master/raw-data/fbref-tm-player-mapping/output/fbref_to_tm_mapping.csv", stringsAsFactors = FALSE)


fbref <- playing_time %>% filter(!Url %in% matched_data$UrlFBref)

fbref <- fbref %>%
  # have made the decision to get rid of players that were listed on team sheets but 
  # haven't yet played as there's too many manual matches of youth players
  filter(!is.na(Min_Playing.Time)) %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Born, Url) %>%
  mutate(fbref_surname = str_squish(gsub(".*\\s", "", Player))) %>%
  arrange(Player)

tm <- tm %>%
  select(comp_name, region, country, season_start_year, squad, player_name, player_position,
         player_dob, player_nationality, player_market_value_euro, player_url) %>%
  arrange(player_name)

# want a df to help with inspection of names with special characters
tm_unique <- tm %>%
  arrange(player_url, desc(season_start_year)) %>% 
  distinct(player_name, player_dob, player_url, .keep_all = TRUE) %>%
  select(player_name, player_dob, player_url, player_position) %>% 
  mutate(tm_surname = str_squish(gsub(".*\\s", "", player_name)),
         tm_yob = lubridate::year(player_dob))

#----- primary join type: -----#
# here I will join the two datasets on the player names
# joined_primary <- fbref %>% select(Player, Born, Url) %>% distinct(Url, .keep_all = T) %>%
#   left_join(tm %>% select(player_name, player_dob, player_url) %>% distinct(player_url, .keep_all = T), by = c("Player" = "player_name"))

joined_primary <- fbref %>% select(Player, Born, Url) %>% distinct(Url, .keep_all = T) %>%
  left_join(tm_unique %>% select(player_name, player_dob, player_url, player_position, tm_yob) %>% distinct(player_url, .keep_all = T),
            by = c("Player" = "player_name", "Born" = "tm_yob"))

# arrange by player name
joined_primary <- joined_primary %>% arrange(Player)


# these players have multiple records in each data set - think "Adama Traoré" or "Rafael" or "Raúl García" for example
# will need to manually go through each of these to map the correct player
duplicate_players <- joined_primary %>% count(Player, Url, sort = T) %>% filter(n > 1) %>% pull(Url)
duplicate_players <- joined_primary %>% filter(Url %in% duplicate_players)


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
joined_secondary <- joined_missing %>%
  mutate(fbref_surname = gsub(".*\\s", "", Player)) %>%
  select(-player_dob, -player_url) %>%
  left_join(tm_unique, by = c("fbref_surname" = "tm_surname", "Born" = "tm_yob", "player_position"))

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
  select(Player, Born, Url, player_dob, player_url)


joined_finished <- joined_complete %>%
  filter(!is.na(player_url)) %>%
  bind_rows(joined_secondary)


# create a file for manual rework by removing any of the records that have been matched since the creation of `joined_missing`:
joined_missing <- joined_missing %>%
  filter(!Url %in% joined_finished$Url,
         !Url %in% duplicate_players$Url)


# write files to work on manually
write.csv(joined_finished, here("raw-data", "fbref-tm-player-mapping", "output", "working-files", "joined_finished.csv"), row.names = F)
write.csv(joined_missing, here("raw-data", "fbref-tm-player-mapping", "output", "working-files", "joined_missing.csv"), row.names = F)
write.csv(tm_unique, here("raw-data", "fbref-tm-player-mapping", "output", "working-files", "tm_unique.csv"), row.names = F)
write.csv(duplicate_players, here("raw-data", "fbref-tm-player-mapping", "output", "working-files", "duplicate_players_df.csv"), row.names = F)
