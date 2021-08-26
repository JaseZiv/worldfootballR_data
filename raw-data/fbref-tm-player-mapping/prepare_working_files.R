library(worldfootballR)
library(tidyverse)
library(here)



fb_big5_advanced_season_stats <- function(season_end_year, stat_type, team_or_player) {

  stat_types <- c("standard", "shooting", "passing", "passing_types", "gca", "defense", "possession", "playing_time", "misc", "keepers", "keepers_adv")

  if(!stat_type %in% stat_types) stop("check stat type")

  print(glue::glue("Scraping {team_or_player} season '{stat_type}' stats"))

  main_url <- "https://fbref.com"

  season_end_year_num <- season_end_year

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  seasons_urls <- seasons %>%
    dplyr::filter(stringr::str_detect(.data$competition_type, "Big 5 European Leagues")) %>%
    dplyr::filter(season_end_year %in% season_end_year_num) %>%
    dplyr::arrange(season_end_year) %>%
    dplyr::pull(seasons_urls) %>% unique()


  get_each_big5_stats_type <- function(season_url) {

    pb$tick()

    if(stat_type == "standard") {
      stat_type <- "stats"
    }

    # fixes the change fbref made with the name of playing time
    if(stat_type == "playing_time") {
      stat_type <- "playingtime"
    }

    # fixes the change fbref made with the name of advanced keepers
    if(stat_type == "keepers_adv") {
      stat_type <- "keepersadv"
    }

    season_stats_page <- xml2::read_html(season_url)

    if(team_or_player == "player") {
      player_squad_ixd <- 1
    } else {
      player_squad_ixd <- 2
    }

    stat_urls <- season_stats_page %>%
      rvest::html_nodes(".hoversmooth") %>%
      rvest::html_nodes(".full") %>%
      rvest::html_nodes("ul") %>% .[player_squad_ixd] %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      paste0(main_url, .)

    stat_urls <- stat_urls[grepl(paste0(stat_type, "/"), stat_urls)]

    team_page <- stat_urls %>%
      xml2::read_html()

    if(team_or_player == "player") {
      stat_df <- team_page %>%
        rvest::html_nodes(".table_container") %>%
        rvest::html_nodes("table") %>%
        rvest::html_table() %>%
        data.frame()
    } else {
      stat_df <- team_page  %>%
        rvest::html_nodes(".table_container") %>%
        rvest::html_nodes("table")

      stat_df_for <- stat_df[1] %>%
        rvest::html_table() %>%
        data.frame()
      stat_df_against <- stat_df[2] %>%
        rvest::html_table() %>%
        data.frame()

      stat_df <- rbind(stat_df_for, stat_df_against)

    }

    var_names <- stat_df[1,] %>% as.character()

    new_names <- paste(var_names, names(stat_df), sep = "_")

    if(stat_type == "playingtime") {
      new_names <- new_names %>%
        gsub("\\..[[:digit:]]+", "", .) %>%
        gsub("\\.[[:digit:]]+", "", .) %>%
        gsub("_Var", "", .) %>%
        gsub("# Pl", "Num_Players", .) %>%
        gsub("%", "_percent", .) %>%
        gsub("_Performance", "", .) %>%
        # gsub("_Penalty", "", .) %>%
        gsub("1/3", "Final_Third", .) %>%
        gsub("/", "_per_", .) %>%
        gsub("-", "_minus_", .) %>%
        gsub("90s", "Mins_Per_90", .) %>%
        gsub("\\+", "plus", .)
    } else {
      new_names <- new_names %>%
        gsub("\\..*", "", .) %>%
        gsub("_Var", "", .) %>%
        gsub("# Pl", "Num_Players", .) %>%
        gsub("%", "_percent", .) %>%
        gsub("_Performance", "", .) %>%
        # gsub("_Penalty", "", .) %>%
        gsub("1/3", "Final_Third", .) %>%
        gsub("/", "_per_", .) %>%
        gsub("-", "_minus_", .) %>%
        gsub("90s", "Mins_Per_90", .)
    }

    names(stat_df) <- new_names
    stat_df <- stat_df[-1,]

    urls <- team_page %>%
      rvest::html_nodes(".table_container") %>%
      rvest::html_nodes("table") %>%
      rvest::html_nodes("tbody") %>%
      rvest::html_nodes("tr") %>% rvest::html_node("td a") %>% rvest::html_attr("href") %>% paste0(main_url, .)

    # important here to change the order of when URLs are applied, so if player, bind before fintering, otherwise after filtering
    # to remove the NAs for the sub heading rows
    if(team_or_player == "player") {
      stat_df <- dplyr::bind_cols(stat_df, Url=urls)

      stat_df <- stat_df %>%
        dplyr::filter(.data$Rk != "Rk") %>%
        dplyr::select(-.data$Rk)

      stat_df <- stat_df %>%
        dplyr::select(-.data$Matches)

      cols_to_transform <- stat_df %>%
        dplyr::select(-.data$Squad, -.data$Player, -.data$Nation, -.data$Pos, -.data$Comp, -.data$Age, -.data$Url) %>% names()

      chr_vars_to_transform <- stat_df %>%
        dplyr::select(.data$Nation, .data$Comp) %>% names()

    } else {
      stat_df <- stat_df %>%
        dplyr::filter(.data$Rk != "Rk") %>%
        dplyr::select(-.data$Rk)

      stat_df <- dplyr::bind_cols(stat_df, Url=urls)

      cols_to_transform <- stat_df %>%
        dplyr::select(-.data$Squad, -.data$Comp, -.data$Url) %>% names()

      chr_vars_to_transform <- stat_df %>%
        dplyr::select(.data$Comp) %>% names()
    }


    stat_df <- stat_df %>%
      dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
      dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
      dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric) %>%
      dplyr::mutate_at(.vars = chr_vars_to_transform, .funs = function(x) {gsub("^\\S* ", "", x)})


    stat_df <- stat_df %>%
      dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data$Squad, "vs "), "team", "opponent")) %>%
      dplyr::filter(.data$Team_or_Opponent == "team") %>%
      dplyr::bind_rows(
        stat_df %>%
          dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data$Squad, "vs "), "team", "opponent")) %>%
          dplyr::filter(.data$Team_or_Opponent == "opponent")
      ) %>%
      dplyr::mutate(season_url = season_url,
                    Squad = gsub("vs ", "", .data$Squad)) %>%
      dplyr::select(.data$season_url, .data$Squad, .data$Comp, .data$Team_or_Opponent, dplyr::everything())


    stat_df <- seasons %>%
      dplyr::select(Season_End_Year=.data$season_end_year, .data$seasons_urls) %>%
      dplyr::left_join(stat_df, by = c("seasons_urls" = "season_url")) %>%
      dplyr::select(-.data$seasons_urls) %>%
      dplyr::filter(!is.na(.data$Squad)) %>%
      dplyr::arrange(.data$Season_End_Year, .data$Squad, dplyr::desc(.data$Team_or_Opponent))

    if(team_or_player == "player") {
      stat_df$Team_or_Opponent <- NULL
    }

    return(stat_df)

  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(seasons_urls))

  all_stats_df <- seasons_urls %>%
    purrr::map_df(get_each_big5_stats_type)

  return(all_stats_df)

}



playing_time <- fb_big5_advanced_season_stats(season_end_year = 2022,
                                              stat_type = "playing_time",
                                              team_or_player = "player")

tm <- get_player_market_values(country_name = c("England", "Spain", "France", "Italy", "Germany"),
                                       start_year = 2021)


matched_data <- read.csv(here("raw-data", "fbref-tm-player-mapping", "output", "initial-match", "fbref_to_tm_up_to_20-21.csv"), stringsAsFactors = FALSE)


fbref <- playing_time %>% filter(!Url %in% matched_data$UrlFBref)

fbref <- fbref %>%
  select(Season_End_Year, Squad, Comp, Player, Nation, Born, Url) %>%
  mutate(fbref_surname = str_squish(gsub(".*\\s", "", Player))) %>%
  arrange(Player)

tm <- tm %>%
  select(comp_name, region, country, season_start_year, squad, player_name, player_position,
         player_dob, player_nationality, player_market_value_euro, player_url) %>%
  arrange(player_name)

# want a df to help with inspection of names with special characters
tm_unique <- tm %>%
  distinct(player_name, player_dob, player_url) %>%
  mutate(tm_surname = str_squish(gsub(".*\\s", "", player_name)),
         tm_yob = lubridate::year(player_dob))

#----- primary join type: -----#
# here I will join the two datasets on the player names
# joined_primary <- fbref %>% select(Player, Born, Url) %>% distinct(Url, .keep_all = T) %>%
#   left_join(tm %>% select(player_name, player_dob, player_url) %>% distinct(player_url, .keep_all = T), by = c("Player" = "player_name"))

joined_primary <- fbref %>% select(Player, Born, Url) %>% distinct(Url, .keep_all = T) %>%
  left_join(tm_unique %>% select(player_name, player_dob, player_url, tm_yob) %>% distinct(player_url, .keep_all = T),
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
