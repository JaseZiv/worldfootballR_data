library(worldfootballR)
library(dplyr)
library(purrr)
library(tidyr)
library(janitor)
library(readr)

source("R/piggyback.R")
matches_by_date <- read_worldfootballr_rds("matches_by_date", tag = "fotmob_matches_by_date")

data_dir <- file.path("data", "fotmob_match_details")
subdata_dir <- file.path(data_dir, "matches")
dir.create(data_dir, showWarnings = FALSE)
dir.create(subdata_dir, showWarnings = FALSE)

scrape_fotmob_match_details <- function(match_id, overwrite = FALSE) {
  rds_path <- file.path(subdata_dir, sprintf("%s.rds", match_id))
  if (file.exists(rds_path) & !overwrite) {
    message(sprintf("Returning pre-saved data for %s.", match_id))
    return(read_rds(rds_path))
  }
  Sys.sleep(1)
  message(sprintf("Scraping matches for %s.", match_id))
  match_details <- fotmob_get_match_details(match_id)
  write_rds(match_details, rds_path)
  match_details
}

# scrape_fotmob_match_details <- function(match_id, overwrite = FALSE) {
#   Sys.sleep(1)
#   message(sprintf("Scraping matches for %s.", match_id))
#   fotmob_get_match_details(match_id)
# }
possibly_scrape_fotmob_match_details <- possibly(scrape_fotmob_match_details, otherwise = tibble(), quiet = FALSE)

league_id_mapping <- tibble(
  league_id = c(48,110,146, 86,140,8972),
  country = c('ENG', 'FRA', 'GER', 'ITA', 'ESP', 'USA'),
  tier = rep('2nd', 6)
)

league_start_dates <- league_id_mapping |> 
  mutate(
    data = map2(
      country, tier,
      ~load_match_results(
        country = ..1, 
        gender = "M", 
        season_end_year = 2021, 
        tier = ..2
      )
    )
  ) |>
  unnest(data) |> 
  group_by(league_id) |> 
  slice_min(Date, n = 1, with_ties = FALSE) |> 
  select(league_id, date = Date)

other_league_ids <- 44

league_start_date_mapping <- setNames(league_start_dates$date, league_start_dates$league_id)

scrape_fotmob_match_details_for_league <- function(league_id) {
  first_date <- if (league_id %in% other_league_ids) {
    as.Date("2020-06-01")
  } else {
    as.Date(league_start_date_mapping[[as.character(league_id)]])
  }
  
  rds_path <- file.path(data_dir, sprintf("%s_match_details.rds", league_id))
  path_exists <- file.exists(rds_path)
  csv_path <- file.path(data_dir, sprintf("%s_match_details.csv", league_id))
  
  if (isTRUE(path_exists)) {
    existing_match_details <- read_rds(rds_path)
    existing_match_ids <- unique(existing_match_details$match_id)
    existing_matches_by_date <- matches_by_date |> 
      filter(!(match_id %in% existing_match_ids))
  } else {
    existing_match_details <- tibble()
    existing_match_ids <- integer()
    existing_matches_by_date <- matches_by_date
  }
  
  new_matches_by_date <- existing_matches_by_date |> 
    filter(primary_id == !!league_id) |> 
    filter(date >= !!first_date) |> 
    filter(!(match_id %in% existing_match_ids)) |> 
    filter(!match_status_cancelled, match_status_finished)
  
  if (nrow(new_matches_by_date) == 0) {
    message(sprintf("Not updating data for `league_id = %s`.", league_id))
    return(existing_match_details)
  }
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  
  new_match_details <- new_matches_by_date$match_id[1:2] |> 
    map_dfr(scrape_fotmob_match_details)
  
  if (nrow(new_match_details) == 0) {
    message(sprintf("Not updating data for `league_id = %s`. Bad matches: %s", league_id, nrow(new_matches_by_date)))
    return(existing_match_details)
  }
  
  match_details <- bind_rows(
    existing_match_details,
    new_match_details
  )
  
  attr(match_details, "scrape_timestamp") <- scrape_time_utc
  write_rds(match_details, rds_path)
  write_csv(match_details, csv_path, na = "")

  # write_worldfootballr_rds_and_csv(
  #   x = match_details,
  #   name = sprintf("%s_match_details", league_id),
  #   tag = "fotmob_match_details"
  # )
  match_details
}

as.character(c(league_id_mapping$league_id, other_league_ids)) |> 
  walk(scrape_fotmob_match_details_for_league)
