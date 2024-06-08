library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)

source(file.path('R', 'piggyback.R'))
source(file.path('R', 'fb_advanced_match_stats', 'shared_fb_advanced_match_stats.R'))

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

scrape_fb_advanced_match_stats <- function(url, stat_type, team_or_player) {
  message(sprintf('Scraping matches for %s.', url))
  fb_advanced_match_stats(
    url, 
    stat_type = stat_type, 
    team_or_player = team_or_player
  )
}

possibly_scrape_fb_advanced_match_stats <- possibly(
  scrape_fb_advanced_match_stats, 
  otherwise = tibble(),
  quiet = FALSE
)

fb_advanced_match_stats_tag <- 'fb_advanced_match_stats'
update_fb_advanced_match_stats <- function(
    country = 'ENG', 
    gender = 'M', 
    tier = '1st', 
    stat_type = 'summary', 
    team_or_player = 'player'
) {
  name <- sprintf('%s_%s_%s_%s_%s_advanced_match_stats', country, gender, tier, stat_type, team_or_player)
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
  
  existing_data <- read_worldfootballr_rds(
    name = name, 
    tag = fb_advanced_match_stats_tag
  )
  existing_match_urls <- unique(existing_data$MatchURL)
  new_match_urls <- setdiff(match_urls, existing_match_urls)
  
  if (length(new_match_urls) == 0) {
    message(
      sprintf('No new match URLs for `country = "%s"`, `gender = "%s"`, `tier = "%s"`, `stat_type = "%s"`, `team_or_player = "%s"`', country, gender, tier, stat_type, team_or_player)
    )
    return(existing_data)
  }
  
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = 'UTC')
  
  new_data <- new_match_urls |> 
    set_names() |> 
    map_dfr(
      \(.x) possibly_scrape_fb_advanced_match_stats(
        url = .x,
        stat_type = stat_type, 
        team_or_player = team_or_player
      ),
      .id = 'MatchURL'
    ) |> 
    relocate(MatchURL, .before = 1)
  
  match_results <- load_match_results(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = filtered_seasons
  )
  
  res <- bind_rows(
    existing_data,
    new_data |> 
      inner_join(
        match_results |> 
          transmute(
            Competition_Name, 
            Gender,
            Country, 
            Tier = .env$tier,
            Season_End_Year, 
            MatchURL
          ),
        by = 'MatchURL'
      )
  ) |> 
    as_tibble()
  
  attr(res, 'scrape_timestamp') <- scrape_time_utc
  
  write_worldfootballr_rds_and_csv(
    x = res, 
    name = name, 
    tag = fb_advanced_match_stats_tag
  )
  
  res
}

params |>  
  crossing(
    stat_type = factor(levels = c('summary', 'passing', 'passing_types', 'defense', 'possession', 'misc', 'keeper')),
    team_or_player = factor(levels = c('team', 'player'))
  ) |> 
  arrange(
    stat_type,
    team_or_player
  ) |> 
  mutate(
    data = pmap(
      list(
        country,
        gender,
        tier,
        stat_type,
        team_or_player
      ),
      ~update_fb_advanced_match_stats(
        country = ..1,
        gender = ..2,
        tier = ..3,
        stat_type = ..4,
        team_or_player = ..5
      )
    )
  )

