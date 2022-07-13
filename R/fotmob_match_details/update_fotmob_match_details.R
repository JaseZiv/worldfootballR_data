library(worldfootballR)
library(dplyr)
library(purrr)
library(tidyr)
library(janitor)

data_dir <- file.path("data", "fotmob_match_details")
subdata_dir <- file.path(data_dir, "matches")
dir.create(data_dir, showWarnings = FALSE)
dir.create(subdata_dir, showWarnings = FALSE)

matches_by_date <- file.path("data", "fotmob_matches_by_date", "matches_by_date.rds") |> readRDS()

scrape_fotmob_match_details <- function(match_id, overwrite = FALSE) {
  path <- file.path(subdata_dir, sprintf("%s.rds", match_id))
  if(file.exists(path) & !overwrite) {
    message(sprintf("Returning pre-saved data for %s.", match_id))
    return(readRDS(path))
  }
  message(sprintf("Scraping matches for %s.", match_id))
  match_details <- fotmob_get_match_details(match_id)
  saveRDS(match_details, path)
  match_details
}
possibly_scrape_fotmob_match_details <- possibly(scrape_fotmob_match_details, otherwise = tibble(), quiet = FALSE)
slowly_scrape_fotmob_match_details <- slowly(possibly_scrape_fotmob_match_details, quiet = TRUE)

league_id_mapping <- c(
  "47" = "ENG",
  "53" = "FRA",
  "54" = "GER",
  "55" = "ITA",
  "87" = "ESP",
  "130" = "USA"
)
other_league_ids <- as.character(c(50, 42, 44, 73))

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

slowly_scrape_fotmob_match_details_for_league <- function(league_id) {
  first_date <- if(league_id %in% other_league_ids) {
    as.Date("2020-06-01")
  } else {
    as.Date(league_start_date_mapping[[league_id]])
  }
  
  path <- file.path(data_dir, sprintf("%s_match_details.rds", league_id))
  path_exists <- file.exists(path)
  if(isTRUE(path_exists)) {
    existing_match_details <- readRDS(path)
    existing_match_ids <- unique(existing_match_details$match_id)
    matches_by_date <- matches_by_date |> 
      filter(!(match_id %in% existing_match_ids))
      
  }

  league_matches_by_date <- matches_by_date |> 
    filter(date >= !!first_date) |> 
    filter(primary_id == !!league_id, !match_status_cancelled)
  
  if(nrow(league_matches_by_date) == 0) {
    message(sprintf("Not updating data for `league_id = %s`.", league_id))
    if(isTRUE(path_exists)) {
      return(existing_match_details)
    } else {
      return(data.frame())
    }
  }
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  
  raw_match_details <- league_matches_by_date$match_id |> 
    map_dfr(slowly_scrape_fotmob_match_details)
  
  if(nrow(raw_match_details) == 0) {
    message(sprintf("Not updating data for `league_id = %s`. Bad matches: %s", league_id, nrow(league_matches_by_date)))
    if(isTRUE(path_exists)) {
      return(existing_match_details)
    } else {
      return(data.frame())
    }
  }
  
  match_details <- raw_match_details |> 
    unnest(shots) |> 
    unnest_wider(on_goal_shot, names_sep = "_") |> 
    clean_names()
  
  if(isTRUE(path_exists)) {
    match_details <- bind_rows(
      existing_match_details,
      match_details
    )
  }
  
  attr(match_details, "scrape_timestamp") <- scrape_time_utc
  saveRDS(match_details, path)
  match_details
}

c(names(league_id_mapping), other_league_ids) |> 
  walk(slowly_scrape_fotmob_match_details_for_league)
