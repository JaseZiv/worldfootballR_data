library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)

source("R/piggyback.R")

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

seasons <- read_csv(
  "https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv"
)

latest_seasons <- seasons |>
  semi_join(
    params,
    by = c("country", "tier", "gender")
  ) |> 
  group_by(country, tier, gender) |>
  slice_max(season_end_year) |> 
  ungroup() |> 
  distinct(
    country,
    gender,
    tier,
    season_end_year
  )


scrape_fb_match_shooting <- function(match_url) {
  message(sprintf("Scraping matches for %s.", match_url))
  fb_match_shooting(match_url)
}

possibly_scrape_fb_match_shooting <- possibly(
  scrape_fb_match_shooting, 
  otherwise = tibble(),
  quiet = FALSE
)

fb_match_shooting_tag <- "fb_match_shooting"
update_fb_match_shooting <- function(country, gender = "M", tier = "1st") {
  name <- sprintf("%s_%s_%s_match_shooting", country, gender, tier)
  message(sprintf("Updating %s.", name))
  
  latest_season <- latest_seasons |> 
    filter(
      country == !!country,
      gender == !!gender,
      tier == !!tier
    ) |> 
    pull(season_end_year)
  
  match_urls <- fb_match_urls(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = latest_season
  )
  
  existing_match_shooting <- read_worldfootballr_rds(
    name = name, 
    tag = fb_match_shooting_tag
  )
  existing_match_urls <- unique(existing_match_shooting$MatchURL)
  new_match_urls <- setdiff(match_urls, existing_match_urls)
  
  if (length(new_match_urls) == 0) {
    message(sprintf('Not updating data for `country = "%s"`, `gender = "%s"`, `tier = "%s"`.', country, gender, tier))
    return(existing_match_shooting)
  }
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  
  new_match_shooting <- new_match_urls |> 
    map_dfr(
      possibly_scrape_fb_match_shooting
    )
  
  match_shooting <- bind_rows(
    existing_match_shooting,
    new_match_shooting
  ) |>
    as_tibble()
  
  attr(match_shooting, "scrape_timestamp") <- scrape_time_utc

  write_worldfootballr(
    x = match_shooting, 
    name = name, 
    tag = fb_match_shooting_tag
  )
  
  match_shooting
}

params |> 
  mutate(
    data = pmap(
      list(
        country,
        gender,
        tier
      ),
      ~update_fb_match_shooting(
        country = ..1,
        gender = ..2,
        tier = ..3
      )
    )
  )

