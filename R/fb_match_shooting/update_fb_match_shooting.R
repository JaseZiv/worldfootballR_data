library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)
library(lubridate)

source(file.path('R', 'piggyback.R'))
source(file.path('R', 'fb_match_shooting', 'shared_fb_match_shooting.R'))

all_seasons <- readr::read_csv(
  'https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv'
)

seasons <- all_seasons |>
  dplyr::semi_join(
    params,
    by = c('country', 'tier', 'gender')
  ) |> 
  dplyr::filter(season_end_year >= 2017L) |> 
  dplyr::distinct(
    country,
    gender,
    tier,
    season_end_year
  )

scrape_fb_match_shooting <- function(match_url) {
  message(sprintf('Scraping matches for %s.', match_url))
  worldfootballR::fb_match_shooting(match_url)
}

possibly_scrape_fb_match_shooting <- purrr::possibly(
  scrape_fb_match_shooting, 
  otherwise = tibble::tibble(),
  quiet = FALSE
)

fb_match_shooting_tag <- 'fb_match_shooting'
update_fb_match_shooting <- function(country, gender = 'M', tier = '1st', date_threshold = 3L) {
  name <- sprintf('%s_%s_%s_match_shooting', country, gender, tier)
  message(sprintf('Updating %s.', name))
  
  filtered_seasons <- seasons |> 
    dplyr::filter(
      country == !!country,
      gender == !!gender,
      tier == !!tier
    ) |> 
    dplyr::pull(season_end_year)
  
  latest_season <- max(filtered_seasons)
  
  match_urls <- worldfootballR::fb_match_urls(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = latest_season
  )
  date_rgx <- sprintf('(%s)-[0-9]{1,2}-20[0-9]{2}', paste0(month.name, collapse = '|'))
  match_names <- basename(match_urls)
  match_dates <- match_names |> 
    # stringr::str_extract() |> 
    regmatches(regexpr(date_rgx, match_names)) |> 
    lubridate::mdy()
  
  current_date <- Sys.Date()
  diffs <- as.integer(lubridate::as.difftime(current_date - match_dates, units = 'days'))
  discarded_match_urls <- match_urls[diffs <= date_threshold]
  retained_match_urls <- match_urls[diffs > date_threshold]
  
  existing_match_shooting <- read_worldfootballr_rds(
    name = name, 
    tag = fb_match_shooting_tag
  )
  existing_match_urls <- unique(existing_match_shooting$MatchURL)
  new_match_urls <- setdiff(retained_match_urls, setdiff(existing_match_urls, discarded_match_urls))
  
  if (length(new_match_urls) == 0) {
    message(sprintf('Not updating data for `country = "%s"`, `gender = "%s"`, `tier = "%s"`.', country, gender, tier))
    return(existing_match_shooting)
  }
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = 'UTC')
  
  new_match_shooting <- new_match_urls |> 
    rlang::set_names() |> 
    purrr::map_dfr(
      possibly_scrape_fb_match_shooting,
      .id = 'MatchURL'
    ) |> 
    dplyr::relocate(MatchURL, .before = 1)

  match_results <- worldfootballR::load_match_results(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = filtered_seasons
  )

  match_shooting <- dplyr::bind_rows(
    existing_match_shooting |> 
      dplyr::filter(!(MatchURL %in% discarded_match_urls)),
    new_match_shooting |> 
      dplyr::inner_join(
        match_results |> 
          dplyr::transmute(
            Competition_Name, 
            Gender,
            Country, 
            Tier = .env$tier,
            Season_End_Year, 
            MatchURL
          )
      )
  ) |> 
    tibble::as_tibble()

  attr(match_shooting, 'scrape_timestamp') <- scrape_time_utc

  write_worldfootballr_rds_and_csv(
    x = match_shooting, 
    name = name, 
    tag = fb_match_shooting_tag
  )
  
  match_shooting
}

params |>
  dplyr::mutate(
    data = purrr::pmap(
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

