library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)

data_dir <- file.path("data", "fb_match_shooting")
subdata_dir <- file.path(data_dir, "matches")
dir.create(data_dir, showWarnings = FALSE)
dir.create(subdata_dir, showWarnings = FALSE)

source(file.path("R", "fb_match_shooting", "shared_fb_match_shooting.R"))

scrape_fb_match_shooting <- function(match_url, overwrite = FALSE) {
  rds_path <- file.path(subdata_dir, sprintf("%s.rds", basename(match_url)))
  if (file.exists(rds_path) & !overwrite) {
    # message(sprintf("Returning pre-saved data for %s.", match_url))
    return(read_rds(rds_path))
  }
  message(sprintf("Scraping matches for %s.", match_url))
  match_shooting <- fb_match_shooting(match_url)
  write_rds(match_shooting, rds_path)
  match_shooting
}

possibly_scrape_fb_match_shooting <- possibly(
  scrape_fb_match_shooting, 
  otherwise = tibble(),
  quiet = FALSE
)

backfill_fb_match_shooting <- function(country, gender = 'M', tier = '1st', group = 'big5') {
  
  rds_path <- file.path(data_dir, sprintf('%s_%s_%s_match_shooting.rds', country, gender, tier))
  message(sprintf('Updating %s.', rds_path))
  path_exists <- file.exists(rds_path)
  
  first_season_end_year <- ifelse(
    group == 'big5',
    2018,
    2019
  )
  
  seasons <- first_season_end_year:2023
  match_urls <- fb_match_urls(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = seasons
  )
  
  if (isTRUE(path_exists)) {
    existing_match_shooting <- read_rds(rds_path)
    existing_match_urls <- unique(existing_match_shooting$MatchURL)
    new_match_urls <- setdiff(match_urls, existing_match_urls)
  } else {
    existing_match_shooting <- tibble()
    new_match_urls <- match_urls
  }
  
  if (length(new_match_urls) == 0) {
    message(sprintf('Not updating data for `country = "%s"`, `gender = "%s"`, `tier = "%s"`.', country, gender, tier))
    return(existing_match_shooting)
  }
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  new_match_shooting <- new_match_urls |> 
    set_names() |> 
    map_dfr(
      possibly_scrape_fb_match_shooting,
      .id = "MatchURL"
    ) |> 
    relocate(MatchURL, .before = 1)
  
  match_results <- load_match_results(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = seasons
  )
  
  match_shooting <- bind_rows(
    existing_match_shooting,
    new_match_shooting
  ) |>
    inner_join(
      match_results |> 
        select(Competition_Name, Gender, Country, Season_End_Year, MatchURL),
      by = "MatchURL"
    ) |> 
    as_tibble()
  
  attr(match_shooting, "scrape_timestamp") <- scrape_time_utc
  write_rds(
    match_shooting, 
    rds_path
  )
  
  invisible(match_shooting)
}

local_data <- params |> 
  mutate(
    data = pmap(
      list(
        country,
        gender,
        tier,
        group
      ),
      ~backfill_fb_match_shooting(
        country = ..1,
        gender = ..2,
        tier = ..3,
        gruop = ..4
      )
    )
  )

## could just put this in the function, but i want to check locally before i upload
source(file.path("R", "piggyback.R"))
local_data |> 
  mutate(
    name = sprintf("%s_%s_%s_match_shooting", country, gender, tier),
    res = map2(
      data,
      name,
      ~{
        write_worldfootballr_rds_and_csv(
          x = .x,
          name = .y,
          tag = "fb_match_shooting"
        )
      }
    )
  )

