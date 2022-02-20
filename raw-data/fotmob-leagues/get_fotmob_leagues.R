
library(rvest)
library(purrr)
library(stringr)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)
library(janitor)
library(here)

page <- 'https://www.fotmob.com/' %>% 
  rvest::read_html()

nested_leagues <- page %>% 
  rvest::html_elements('script') %>% 
  rvest::html_text(trim = TRUE) %>% 
  purrr::keep(stringr::str_detect, 'lsLeague') %>% 
  stringr::str_extract(
    '(?<=\\"countries\\"[:]).*(?=\\,\\"userSettings\\":null[}]\\,\\"userSettings\\")'
  ) %>% 
  jsonlite::fromJSON() %>% 
  tidyr::as_tibble()

leagues <- nested_leagues %>%
  dplyr::rename(
    country = name
  ) %>% 
  tidyr::unnest(leagues) %>% 
  janitor::clean_names()

write.csv(
  leagues, 
  here::here('raw-data', 'fotmob-leagues', 'all_leagues.csv'), 
  row.names = FALSE
)
