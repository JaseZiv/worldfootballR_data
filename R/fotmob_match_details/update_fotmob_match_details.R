library(worldfootballR)
library(dplyr)
library(purrr)
library(tidyr)
library(janitor)
library(readr)

source("R/piggyback.R")

data_dir <- file.path("data", "fotmob_match_details")
dir.create(data_dir, showWarnings = FALSE)

matches_by_date <- read_worldfootballr_rds("matches_by_date", tag = "fotmob_matches_by_date")

scrape_fotmob_match_details <- function(match_id, overwrite = FALSE) {
  Sys.sleep(1)
  message(sprintf("Scraping matches for %s.", match_id))
  fotmob_get_match_details(match_id)
}
possibly_scrape_fotmob_match_details <- possibly(scrape_fotmob_match_details, otherwise = tibble(), quiet = FALSE)

league_id_mapping <- c(
  "47" = "ENG",
  "53" = "FRA",
  "54" = "GER",
  "55" = "ITA",
  "87" = "ESP",
  "130" = "USA"
)
other_league_ids <- as.character(c(50, 42, 73))

league_start_dates <- league_id_mapping |> 
  imap_dfr(
    ~load_match_results(
      country = .x, 
      gender = "M", 
      season_end_year = 2021, 
      tier = "1st"
    ) |> 
      slice_min(Date, n = 1, with_ties = FALSE) |> 
      select(date = Date) |> 
      mutate(league_id = .y, .before = 1)
  )
league_start_date_mapping <- setNames(league_start_dates$date, league_start_dates$league_id)

scrape_fotmob_match_details_for_league <- function(league_id) {
  first_date <- if (league_id %in% other_league_ids) {
    as.Date("2020-06-01")
  } else {
    as.Date(league_start_date_mapping[[league_id]])
  }
  
  existing_match_details <- read_worldfootballr_rds(
    sprintf("%s_match_details", league_id), 
    tag = "fotmob_match_details"
  )
  existing_match_ids <- unique(existing_match_details$match_id)
  
  new_matches_by_date <- matches_by_date |> 
    filter(primary_id == !!league_id) |> 
    filter(date >= !!first_date) |> 
    filter(!(match_id %in% existing_match_ids)) |> 
    filter(!match_status_cancelled, match_status_finished)
  
  if (nrow(new_matches_by_date) == 0) {
    message(sprintf("Not updating data for `league_id = %s`.", league_id))
    return(existing_match_details)
  }
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  
  new_match_details <- new_matches_by_date$match_id |> 
    map_dfr(possibly_scrape_fotmob_match_details)
  
  if (nrow(existing_match_details) == 0) {
    message(sprintf("Not updating data for `league_id = %s`. Bad matches: %s", league_id, nrow(new_matches_by_date)))
    return(existing_match_details)
  }
  
  match_details <- bind_rows(
    existing_match_details,
    new_match_details
  )
  
  attr(match_details, "scrape_timestamp") <- scrape_time_utc
  ## TODO: Remove write_rds/csv2 lines
  rds_path <- file.path(data_dir, sprintf("%s_match_details.rds", league_id))
  csv_path <- file.path(data_dir, sprintf("%s_match_details.csv", league_id))
  write_rds(match_details, rds_path)
  write_csv(match_details, csv_path, na = "")
  write_worldfootballr_rds_and_csv(
    x = match_details,
    name = sprintf("%s_match_details", league_id),
    tag = "fotmob_match_details"
  )
  match_details
}

c(names(league_id_mapping), other_league_ids) |> 
  walk(scrape_fotmob_match_details_for_league)
