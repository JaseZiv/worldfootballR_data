library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)

source(file.path('R', 'piggyback.R'))
source(file.path('R', 'fb_advanced_match_stats', 'shared_fb_advanced_match_stats.R'))

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

scrape_fb_advanced_match_stats <- function(url, stat_type, team_or_player) {
  message(
    sprintf(
      'Scraping matches for url = `"%s"`,\n`stat_type = "%s"`, `team_or_player = "%s"`.', 
      url, 
      stat_type,
      team_or_player
    )
  )
  worldfootballR::fb_advanced_match_stats(
    url, 
    stat_type = stat_type, 
    team_or_player = team_or_player
  )
}

possibly_scrape_fb_advanced_match_stats <- purrr::possibly(
  scrape_fb_advanced_match_stats, 
  otherwise = tibble::tibble(),
  quiet = FALSE
)

slowly_possibly_scrape_fb_advanced_match_stats <- purrr::slowly(
  possibly_scrape_fb_advanced_match_stats, 
  rate = purrr::rate_delay(pause = 5),
  quiet = TRUE
)

bind_with_type_coercion <- function(df1, df2) {
  common_cols <- intersect(names(df1), names(df2))
  
  class_df1 <- sapply(df1[common_cols], class)
  class_df2 <- sapply(df2[common_cols], class)
  
  cols_to_coerce <- common_cols[class_df1 != class_df2]
  
  if (length(cols_to_coerce) > 0) {
    message(
      sprintf('Coerceing these columns to strings: `%s`', paste0(cols_to_coerce, collapse = '`, `'))
    )
    df1[cols_to_coerce] <- lapply(df1[cols_to_coerce], as.character)
    df2[cols_to_coerce] <- lapply(df2[cols_to_coerce], as.character)
  }
  
  dplyr::bind_rows(df1, df2)
}

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
  
  existing_data <- read_worldfootballr(
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
    rlang::set_names() |> 
    purrr::map_dfr(
      \(.x) slowly_possibly_scrape_fb_advanced_match_stats(
        url = .x,
        stat_type = stat_type, 
        team_or_player = team_or_player
      ),
      .id = 'MatchURL'
    ) |> 
    dplyr::relocate(MatchURL, .before = 1)
  
  match_results <- worldfootballR::load_match_results(
    country = country,
    tier = tier,
    gender = gender,
    season_end_year = filtered_seasons
  )
  
  res <- bind_with_type_coercion(
    existing_data,
    new_data |> 
      dplyr::inner_join(
        match_results |> 
          dplyr::transmute(
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
    tibble::as_tibble()
  
  attr(res, 'scrape_timestamp') <- scrape_time_utc
  
  write_worldfootballr_rds_and_csv(
    x = res, 
    name = name, 
    tag = fb_advanced_match_stats_tag
  )
  
  res
}

current_time <- lubridate::now(tzone = 'UTC')
current_wday <- lubridate::wday(current_time)
current_hour <- lubridate::hour(current_time)

team_or_players <- if (current_wday %% 2 == 0) {
  'player'
} else {
  'team'
}

stat_types <- if (current_hour <= 12) {
  c('summary', 'passing', 'passing_types')
} else {
  c('defense', 'possession', 'misc', 'keeper')
}

params |>
  tidyr::crossing(
    stat_type = factor(stat_types, levels = c('summary', 'passing', 'passing_types', 'defense', 'possession', 'misc', 'keeper')),
    team_or_player = factor(team_or_players, levels = c('team', 'player'))
  ) |> 
  dplyr::arrange(
    stat_type,
    team_or_player
  ) |> 
  dplyr::mutate(
    data = purrr::pmap(
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

