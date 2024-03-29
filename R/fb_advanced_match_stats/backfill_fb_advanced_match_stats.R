library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)

PARENT_DATA_DIR <- file.path('data', 'fb_advanced_match_stats')
SUB_DATA_DIR <- file.path(PARENT_DATA_DIR, 'matches')
dir.create(SUB_DATA_DIR, showWarnings = FALSE, recursive = FALSE)

source(file.path('R', 'fb_advanced_match_stats', 'shared_fb_advanced_match_stats.R'))

scrape_fb_advanced_match_stats <- function(url, stat_type, team_or_player, data_dir, overwrite = FALSE) {
  rds_path <- file.path(data_dir, stat_type, team_or_player, sprintf('%s.rds', basename(url)))
  if (!dir.exists(dirname(rds_path))) { dir.create(dirname(rds_path), showWarnings = FALSE, recursive = TRUE) }
  
  suffix <- sprintf('for `stat_type = "%s"`, `team_or_player = "%s", `url = "%s"`.', stat_type, team_or_player, url)
  if (file.exists(rds_path) & !overwrite) {
    return(readr::read_rds(rds_path))
  }
  message(sprintf('Scraping data %s', suffix))
  res <- worldfootballR::fb_advanced_match_stats(url, stat_type = stat_type, team_or_player = team_or_player)
  readr::write_rds(res, rds_path)
  res
}

possibly_scrape_fb_advanced_match_stats <- purrr::possibly(
  scrape_fb_advanced_match_stats, 
  otherwise = tibble::tibble(),
  quiet = FALSE
)

backfill_fb_advanced_match_stats <- function(
    country = 'ENG',
    gender = 'M', 
    tier = '1st', 
    group = 'big5',
    stat_type = 'summary', 
    team_or_player = 'team'
) {
  
  rds_path <- file.path(PARENT_DATA_DIR, sprintf('%s_%s_%s_%s_%s_advanced_match_stats.rds', country, gender, tier, stat_type, team_or_player))
  message(sprintf('Updating %s.', rds_path))
  path_exists <- file.exists(rds_path)
  
  if (isTRUE(path_exists)) {
    existing_data <- readr::read_rds(rds_path)
    existing_match_urls <- unique(existing_data$MatchURL)
  } else {
    existing_data <- tibble::tibble()
  }
  
  first_season_end_year <- ifelse(
    group == 'big5',
    2018,
    2019
  )
  
  ## special logic if this is the MLS before Feb.
  last_season_end_year <- lubridate::year(Sys.Date()) + ifelse(country == 'ENG' & gender == 'M', 0L, 0L)
  season_end_years <- first_season_end_year:last_season_end_year
  
  res <- purrr::map_dfr(
    season_end_years,
    function(season_end_year) {
      
      message(sprintf('Scraping %s.', season_end_year))
      season_path <- file.path(SUB_DATA_DIR, country, gender, tier, season_end_year, paste0(stat_type, '-', team_or_player, '.rds'))
      long_ago <- season_end_year < last_season_end_year
      if (long_ago & file.exists(season_path)) {
        return(readRDS(season_path))
      }
      
      match_urls <- worldfootballR::fb_match_urls(
        country = country,
        tier = tier,
        gender = gender,
        season_end_year = season_end_year
      )
      
      if (length(match_urls) == 0) {
        warning(
          sprintf('No match URLs for `country = "%s"`, `gender = "%s"`, `tier = "%s"`, `season_end_year = %s`, `stat_type = "%s"`, `team_or_player = "%s"`', country, gender, tier, season_end_year, stat_type, team_or_player)
        )
        return(tibble::tibble())
      }
      
      new_data <- match_urls |> 
        rlang::set_names() |> 
        purrr::map_dfr(
          \(.x) possibly_scrape_fb_advanced_match_stats(
            url = .x,
            stat_type = stat_type, 
            team_or_player = team_or_player, 
            data_dir = file.path(SUB_DATA_DIR, country, gender, tier, season_end_year)
          ),
          .id = 'MatchURL'
        ) |> 
        dplyr::relocate(MatchURL, .before = 1)
      
      ## for the URLs
      match_results <- worldfootballR::load_match_results(
        country = country,
        tier = tier,
        gender = gender,
        season_end_year = season_end_year
      )
      
      res <- new_data |> 
        dplyr::select(
          -dplyr::any_of(
            c(
              "Competition_Name", 
              "Gender", 
              "Country", 
              "Tier", 
              "Season_End_Year"
            )
          )
        ) |>
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
          by = dplyr::join_by(MatchURL)
        ) |> 
        tibble::as_tibble()
      saveRDS(res, season_path)
      res
    }
  )
  
  attr(res, 'scrape_timestamp') <- as.POSIXlt(Sys.time(), tz = 'UTC')
  readr::write_rds(
    res, 
    rds_path
  )
  
  invisible(res)
}

local_data <- params |> 
  tidyr::crossing(
    stat_type = c('summary', 'passing', 'passing_types', 'defense', 'possession', 'misc', 'keeper'),
    team_or_player = 'team'
  ) |> 
  dplyr::mutate(
    data = purrr::pmap(
      list(
        country,
        gender,
        tier,
        group,
        stat_type,
        team_or_player
      ),
      ~backfill_fb_advanced_match_stats(
        country = ..1,
        gender = ..2,
        tier = ..3,
        group = ..4,
        stat_type = ..5,
        team_or_player = ..6
      )
    )
  )

## could just put this in the function, but i want to check locally before i upload
source(file.path('R', 'piggyback.R'))
local_data |> 
  mutate(
    name = sprintf('%s_%s_%s_%s_%s_advanced_match_stats', country, gender, tier, stat_type, team_or_player),
    res = map2(
      data,
      name,
      ~{
        write_worldfootballr_rds_and_csv(
          x = .x,
          name = .y,
          tag = 'fb_advanced_match_stats'
        )
      }
    )
  )
