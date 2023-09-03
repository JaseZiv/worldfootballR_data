library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)

source(file.path('R', 'piggyback.R'))
source(file.path('R', 'fb_match_summary', 'shared_fb_match_summary.R'))

all_seasons <- read_csv(
  'https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv'
)

seasons <- all_seasons |>
  semi_join(
    params,
    by = c('country', 'tier', 'gender')
  ) |> 
  filter(season_end_year >= 2017L) |> 
  distinct(
    country,
    gender,
    tier,
    season_end_year
  )

scrape_fb_match_summary <- function(match_url) {
  message(sprintf('Scraping matches for %s.', match_url))
  fb_match_summary(match_url)
}

possibly_scrape_fb_match_summary <- possibly(
  scrape_fb_match_summary, 
  otherwise = tibble(),
  quiet = FALSE
)

fb_match_summary_tag <- 'fb_match_summary'
update_fb_match_summary <- function(country, gender = 'M', tier = '1st') {
  name <- sprintf('%s_%s_%s_match_summary', country, gender, tier)
  message(sprintf('Updating %s.', name))
  
  filtered_seasons <- seasons |> 
    filter(
      country == !!country,
      gender == !!gender,
      tier == !!tier
    ) |> 
    pull(season_end_year)
  
  latest_season <- max(filtered_seasons)
  
  match_urls <- fb_match_urls(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = latest_season
  )
  
  existing_match_summary <- read_worldfootballr_rds(
    name = name, 
    tag = fb_match_summary_tag
  )
  existing_match_urls <- unique(existing_match_summary$MatchURL)
  new_match_urls <- setdiff(match_urls, existing_match_urls)
  
  if (length(new_match_urls) == 0) {
    message(sprintf('Not updating data for `country = "%s"`, `gender = "%s"`, `tier = "%s"`.', country, gender, tier))
    return(existing_match_summary)
  }
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = 'UTC')
  
  new_match_summary <- new_match_urls |> 
    set_names() |> 
    map_dfr(
      possibly_scrape_fb_match_summary,
      .id = 'MatchURL'
    ) |> 
    relocate(MatchURL, .before = 1)
  
  match_results <- load_match_results(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = filtered_seasons
  )
  
  match_summary <- bind_rows(
    existing_match_summary,
    new_match_summary |> 
      inner_join(
        match_results |> 
          transmute(
            Competition_Name, 
            Gender,
            Country, 
            Tier = .env$tier,
            Season_End_Year, 
            MatchURL
          )
      )
  ) |> 
    as_tibble()
  
  attr(match_summary, 'scrape_timestamp') <- scrape_time_utc
  
  write_worldfootballr_rds_and_csv(
    x = match_summary, 
    name = name, 
    tag = fb_match_summary_tag
  )
  
  match_summary
}

params |>
  mutate(
    data = pmap(
      list(
        country,
        gender,
        tier
      ),
      ~update_fb_match_summary(
        country = ..1,
        gender = ..2,
        tier = ..3
      )
    )
  )

