
library(dplyr)
library(jsonlite)
library(stringr)
library(tibble)
library(tidyr)

## Can use any team id here. (This is Liverpool)
resp <- "https://www.fotmob.com/teams?id=8650&tab=stats" %>% 
  jsonlite::fromJSON()
stats <- resp$stats
.extract_data_url <- function(element) {
  stats[[element]]$fetchAllUrl %>%
    basename() %>%
    stringr::str_remove("[.]json")
}

raw_fotmob_stats <- list(
  player = .extract_data_url("players"),
  team = .extract_data_url("teams")
) %>%
  tibble::enframe("entity", "full_stat") %>%
  tidyr::unnest_longer(full_stat) %>% 
  dplyr::arrange(entity, full_stat)

write.csv(
  raw_fotmob_stats, 
  here::here("raw-data", "fotmob-stats", "raw_stat_types.csv"), 
  row.names = FALSE
)

## Have to wrangle some of the above output manually.