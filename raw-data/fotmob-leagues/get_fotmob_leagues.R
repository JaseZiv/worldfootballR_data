library(rvest)
library(purrr)
library(stringr)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)
library(janitor)
library(here)

page <- "https://www.fotmob.com/" %>% 
  rvest::read_html()

league_element <- page %>% 
  rvest::html_elements("script") %>% 
  rvest::html_text(trim = TRUE) %>% 
  purrr::keep(stringr::str_detect, "lsLeague")

all_leagues <- league_element %>% 
  stringr::str_extract(
    '(?<=\\"countries\\"[:]).*(?=\\,\\"userSettings\\":null[}]\\,\\"userSettings\\")'
  ) %>% 
  jsonlite::fromJSON() %>% 
  tibble::as_tibble() %>%
  dplyr::rename(
    country = name
  ) %>% 
  tidyr::unnest(leagues) %>% 
  janitor::clean_names()

popular_leagues <- league_element %>% 
  stringr::str_extract(
    '(?<=\\"popular\\"[:]).*(?=\\,\\"international\\")'
  ) %>% 
  jsonlite::fromJSON() %>% 
  tibble::as_tibble() %>% 
  janitor::clean_names()

write.csv(
  all_leagues, 
  here::here("raw-data", "fotmob-leagues", "all_leagues.csv"), 
  row.names = FALSE
)

## Manually go to league page -> stats tab -> (See all) under any stat card -> extract season id from the url
## This is the only way that I could find season ids. This is needed to get the list of season ids for select leagues.
# nested_leagues$page_url %>% basename() %>% clipr::write_clip()
season_ids_2022 <- c(
  "EURO" = 16215,
  "Champions League" = 16407,
  "Copa America" = 15148,
  "Europa League" = 16640,
  "Premier League" = 16390,
  "1. Bundesliga" = 16494,
  "LaLiga" = 16520,
  "Ligue 1" = 16499,
  "MLS" = 17185,
  "Serie A" = 16621
)

extract_season_ids_for_popular_league <- function(league_name) {
  popular_league <- popular_leagues %>% filter(name == league_name)

  page <- sprintf(
    "https://www.fotmob.com/leagues/%s/stats/season/%s/players/goals/%s",
    popular_league$id,
    season_ids_2022[[league_name]],
    basename(popular_league$page_url)
  ) %>% 
    rvest::read_html()
  
  page %>% 
    rvest::html_elements("script") %>% 
    rvest::html_text(trim = TRUE) %>% 
    purrr::keep(stringr::str_detect, "lsLeague") %>% 
    stringr::str_extract(
      '(?<=\\"leagueDetails\\"[:]).*(?=\\,\\"type\\")'
    ) %>% 
    jsonlite::fromJSON() %>% 
    tibble::as_tibble() %>% 
    janitor::clean_names() %>% 
    dplyr::rename(ccode = country_code, season = seasons) %>% 
    tidyr::unnest_wider(season, names_sep = "_")
}

popular_season_ids <- season_ids_2022 %>% 
  names() %>% 
  purrr::map_dfr(
    extract_season_ids_for_popular_league
  )

write.csv(
  popular_season_ids, 
  here::here("raw-data", "fotmob-leagues", "season_ids.csv"), 
  row.names = FALSE
)

