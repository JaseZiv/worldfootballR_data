library(worldfootballR)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(janitor)
library(readr)

source("R/piggyback.R")

scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
yesterday <- Sys.Date() - 1

dates <- seq.Date(as.Date("2017-08-11"), yesterday, by = "day")
existing_matches_by_date <- read_worldfootballr_rds("matches_by_date", tag = "fotmob_matches_by_date")
existing_dates <- existing_matches_by_date |> 
  distinct(date) |> 
  pull(date) |> 
  ymd()
new_dates <- as.Date(setdiff(dates, existing_dates), origin = "1970-01-01")

if (length(new_dates) == 0) {
  message("Skipping since there are no new dates.")
} else {
  
  scrape_fotmob_matches_on_date <- function(date, overwrite = FALSE) {
    Sys.sleep(1)
    message(sprintf("Scraping matches for %s.", date))
    fotmob_get_matches_by_date(date)
  }
  possibly_scrape_fotmob_matches_on_date <- possibly(scrape_fotmob_matches_on_date, otherwise = tibble(), quiet = FALSE)
  
  new_matches_by_date <- new_dates |> 
    set_names() |> 
    map_dfr(
      possibly_scrape_fotmob_matches_on_date,
      .id = "date"
    )
  
  matches_by_date <- bind_rows(
    existing_matches_by_date,
    new_matches_by_date
  ) |> 
    distinct()
  
  popular_league_ids <- c(50, 42, 44, 73, 47, 54, 87, 53, 130, 55)
  tier2_big5_and_mls_ids <- c(48, 110, 146, 86, 140, 8972)
  all_league_ids <- c(popular_league_ids, tier2_big5_and_mls_ids)
  walk(
    all_league_ids,
    ~{
      new_league_matches_by_date <- new_matches_by_date |> filter(primary_id == .x)
      if (nrow(new_league_matches_by_date) == 0) {
        return(invisible())
      }
      league_matches_by_date <- matches_by_date |> filter(primary_id == .x)
      attr(.x, "scrape_timestamp") <- scrape_time_utc
      write_worldfootballr_rds_and_csv(
        x = league_matches_by_date,
        name = sprintf("%s_matches_by_date", .x),
        tag = "fotmob_matches_by_date"
      )
    }
  )

  attr(matches_by_date, "scrape_timestamp") <- scrape_time_utc
  write_worldfootballr_rds_and_csv(
    matches_by_date,
    name = "matches_by_date",
    tag = "fotmob_matches_by_date"
  )
}
