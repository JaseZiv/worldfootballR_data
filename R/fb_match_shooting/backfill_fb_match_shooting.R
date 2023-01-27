library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)

data_dir <- file.path("data", "fb_match_shooting")
subdata_dir <- file.path(data_dir, "matches")
dir.create(data_dir, showWarnings = FALSE)
dir.create(subdata_dir, showWarnings = FALSE)

params <- crossing(
  country = c(
    "ENG",
    "ESP",
    "FRA",
    "GER",
    "ITA",
    "USA"
  ),
  tier = "1st",
  gender = "M"
)

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

backfill_fb_match_shooting <- function(country, gender = "M", tier = "1st") {

  rds_path <- file.path(data_dir, sprintf("%s_%s_%s_match_shooting.rds", country, gender, tier))
  path_exists <- file.exists(rds_path)
  
  match_urls <- fb_match_urls(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = 2018:2023
  )

  if (isTRUE(path_exists)) {
    existing_match_shooting <- read_rds(rds_path)
    existing_match_urls <- unique(existing_match_shooting$match_url)
    new_match_urls <- setdiff(match_urls, existing_match_urls)
  } else {
    existing_match_shooting <- tibble()
    new_match_urls <- match_urls
  }

  if (length(new_match_urls) == 0) {
    message(sprintf('Not updating data for `country = "%s"`, `gender = "%s"`, `tier = "%s"`.', country, gender, tier))
    return(existing_match_shooting)
  }

  new_match_shooting <- new_match_urls |> 
    map_dfr(
      possibly_scrape_fb_match_shooting
    )
  
  match_shooting <- bind_rows(
    # existing_match_shooting,
    new_match_shooting
  ) |>
    as_tibble()
  
  write_rds(
    match_shooting, 
    rds_path
  )
  
  invisible(match_shooting)
}

local_data <- params |> 
  transmute(
    data = pmap(
      list(
        country,
        gender,
        tier
      ),
      ~backfill_fb_match_shooting(
        country = ..1,
        gender = ..2,
        tier = ..3
      )
    )
  )

## could just put this in the function, but i want to check locally before i upload
source("R/piggyback.R")
local_data |> 
  mutate(
    name = sprintf("%s_%s_%s_match_shooting", country, gender, tier),
    res = map2(
      data,
      name,
      ~{
        write_worldfootballr(
          ext = "csv",
          x = .x |> select(-c(country, gender, tier)),
          name = .y,
          tag = "fb_match_shooting"
        )
      }
    )
  )

