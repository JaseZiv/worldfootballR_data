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

scrape_fotmob_matches_on_date <- function(date, overwrite = FALSE) {
  Sys.sleep(1)
  message(sprintf("Scraping matches for %s.", date))
  fotmob_get_matches_by_date(date)
}

possibly_scrape_fotmob_matches_on_date <- possibly(scrape_fotmob_matches_on_date, otherwise = tibble(), quiet = FALSE)

matches_by_date <- dates |> 
  set_names() |> 
  map_dfr(
    possibly_scrape_fotmob_matches_on_date,
    .id = "date"
  )

ids_to_backfill <- c(48, 110, 146, 86, 140, 8972)
walk(
  ids_to_backfill,
  ~{
    league_matches_by_date <- all_matches_by_date |> filter(primary_id == .x)
    if (nrow(league_matches_by_date) == 0) {
      message("No matches.")
      return(invisible())
    }
    attr(.x, "scrape_timestamp") <- scrape_time_utc
    write_worldfootballr_rds_and_csv(
      x = league_matches_by_date,
      name = sprintf("%s_matches_by_date", .x),
      tag = "fotmob_matches_by_date"
    )
  }
)

# attr(matches_by_date, "scrape_timestamp") <- scrape_time_utc
# write_worldfootballr(
#   matches_by_date,
#   name = "matches_by_date",
#   tag = "fotmob_matches_by_date",
#   ext = "rds"
# )