library(worldfootballR)
library(dplyr)
library(readr)
library(purrr)
library(tibble)

source("R/piggyback.R")

countries <- c(
  "ENG",
  "ESP",
  "FRA",
  "GER",
  "ITA",
  "USA"
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
update_fb_match_shooting <- function(country, gender = "M", tier = "1st")) {
  name <- sprintf("%s_%s_%s_match_shooting", country, gender, tier)
  existing_match_shooting <- read_worldfootballr_rds(
    name = name, 
    tag = fb_match_shooting_tag
  )
  existing_match_shooting_urls <- unique(existing_match_shooting$match_url)
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
  ) %>%
    as_tibble()
  
  attr(match_shooting, "scrape_timestamp") <- scrape_time_utc
  write_worldfootballr(
    x = match_shooting, 
    name = name, 
    tag = fb_match_shooting_tag
    ext = "rds"
  )
  
  match_shooting
}

walk(
  countries,
  update_fb_match_shooting
)
