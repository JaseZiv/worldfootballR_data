library(worldfootballR)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(janitor)

data_dir <- file.path("data", "fotmob_matches_by_date")
subdata_dir <- file.path(data_dir, "dates")
dir.create(data_dir, showWarnings = FALSE)
dir.create(subdata_dir, showWarnings = FALSE)

scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
path <- file.path(data_dir, "matches_by_date.rds")
path_exists <- file.exists(path)
yesterday <- Sys.Date() - 1

dates <- seq.Date(as.Date("2017-08-11"), yesterday, by = "day")
if (isTRUE(path_exists)) {
  existing_matches_by_date <- readRDS(path)
  existing_dates <- existing_matches_by_date |> 
    distinct(date) |> 
    pull(date) |> 
    ymd()
  dates <- as.Date(setdiff(dates, existing_dates), origin = "1970-01-01")
}

scrape_fotmob_matches_on_date <- function(date, overwrite = FALSE) {
  path <- file.path(subdata_dir, sprintf("%s.rds", date))
  if(file.exists(path) & !overwrite) {
    message(sprintf("Returning pre-saved data for %s.", date))
    return(readRDS(path))
  }
  message(sprintf("Scraping matches for %s.", date))
  matches <- fotmob_get_matches_by_date(date)
  saveRDS(matches, path)
  matches
}
possibly_scrape_fotmob_matches_on_date <- possibly(scrape_fotmob_matches_on_date, otherwise = tibble(), quiet = FALSE)
slowly_scrape_fotmob_matches_on_date <- slowly(possibly_scrape_fotmob_matches_on_date, quiet = TRUE)

raw_matches_by_date <- setNames(dates, dates) |> 
  map_dfr(
    slowly_scrape_fotmob_matches_on_date,
    .id = "date"
  )

matches_by_date <- raw_matches_by_date |>   
  rename(match = matches) |> 
  unnest(match, names_sep = "_") |> 
  rename(home = match_home, away = match_away) |> 
  unnest(c(home, away, match_status), names_sep = "_") |> 
  unnest(c(match_status_reason)) |> 
  clean_names()

if(isTRUE(path_exists)) {
  matches_by_date <- bind_rows(
    existing_matches_by_date |> filter(!(date %in% dates)),
    matches_by_date
  ) |> 
    distinct()
}

popular_league_ids <- c(50, 42, 44, 73, 47, 54, 87, 53, 130, 55)
popular_matches_by_date <- matches_by_date |> 
  filter(primary_id %in% popular_league_ids)

split(popular_matches_by_date, popular_matches_by_date$primary_id) |> 
  iwalk(
    ~{
      attr(.x, "scrape_timestamp") <- scrape_time_utc
      saveRDS(.x, file.path(data_dir, sprintf("%s_matches_by_date.rds", .y)))
    }
  )
attr(matches_by_date, "scrape_timestamp") <- scrape_time_utc
saveRDS(matches_by_date, path)
