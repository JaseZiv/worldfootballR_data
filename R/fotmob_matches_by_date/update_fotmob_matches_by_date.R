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
dates <- as.Date(setdiff(dates, existing_dates), origin = "1970-01-01")

scrape_fotmob_matches_on_date <- function(date, overwrite = FALSE) {
  Sys.sleep(1)
  message(sprintf("Scraping matches for %s.", date))
  fotmob_get_matches_by_date(date)
}
possibly_scrape_fotmob_matches_on_date <- possibly(scrape_fotmob_matches_on_date, otherwise = tibble(), quiet = FALSE)

matches_by_date <- setNames(dates, dates) |> 
  map_dfr(
    possibly_scrape_fotmob_matches_on_date,
    .id = "date"
  )

matches_by_date <- bind_rows(
  existing_matches_by_date |> filter(!(date %in% dates)),
  matches_by_date
) |> 
  distinct()

popular_league_ids <- c(50, 42, 73, 47, 54, 87, 53, 130, 55)
popular_matches_by_date <- matches_by_date |> 
  filter(primary_id %in% popular_league_ids)

split(popular_matches_by_date, popular_matches_by_date$primary_id) |> 
  iwalk(
    ~{
      attr(.x, "scrape_timestamp") <- scrape_time_utc
      write_worldfootballr_rds_and_csv(
        x = .x,
        name = sprintf("%s_matches_by_date", .y),
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
