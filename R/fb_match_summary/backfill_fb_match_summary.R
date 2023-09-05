library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)

PARENT_DATA_DIR <- file.path('data', 'fb_match_summary')
SUB_DATA_DIR <- file.path(PARENT_DATA_DIR, 'matches')
dir.create(SUB_DATA_DIR, showWarnings = FALSE, recursive = FALSE)

source(file.path('R', 'fb_match_summary', 'shared_fb_match_summary.R'))

scrape_fb_match_summary <- function(url, data_dir, overwrite = FALSE) {
  rds_path <- file.path(data_dir, sprintf('%s.rds', basename(url)))
  if (!dir.exists(dirname(rds_path))) { dir.create(dirname(rds_path), showWarnings = FALSE, recursive = TRUE) }
  
  suffix <- sprintf('for `url = "%s"`.', url)
  if (file.exists(rds_path) & !overwrite) {
    return(readr::read_rds(rds_path))
  }
  message(sprintf('Scraping data %s', suffix))
  stats <- worldfootballR::fb_match_summary(url)
  readr::write_rds(stats, rds_path)
  stats
}

possibly_scrape_fb_match_summary <- purrr::possibly(
  scrape_fb_match_summary, 
  otherwise = tibble::tibble(),
  quiet = FALSE
)

backfill_fb_match_summary <- function(
    country = 'ENG',
    gender = 'M', 
    tier = '1st', 
    group = 'big5'
) {
  
  rds_path <- file.path(PARENT_DATA_DIR, sprintf('%s_%s_%s_match_summary.rds', country, gender, tier))
  message(sprintf('Updating %s.', rds_path))

  first_season_end_year <- ifelse(
    group == 'big5',
    2018,
    2019
  )
  
  last_season_end_year <- ifelse(country %in% c('USA', 'BRA'), 2023, 2024)
  season_end_years <- first_season_end_year:last_season_end_year

  res <- purrr::map_dfr(
    season_end_years,
    function(season_end_year) {

      season_path <- file.path(SUB_DATA_DIR, country, gender, tier, paste0(season_end_year, '.rds'))
      if (season_end_year < last_season_end_year & file.exists(season_path)) {
        return(readRDS(season_path))
      }
      
      match_urls <- worldfootballR::fb_match_urls(
        country = country,
        tier = tier,
        gender = gender,
        season_end_year = season_end_year
      )
      
      if (is.null(match_urls)) {
        warning(
          sprintf('No match URLs for `country = "%s"`, `gender = "%s"`, `tier = "%s"`, `season_end_year = %s`.', country, gender, tier, season_end_year)
        )
        return(tibble::tibble())
      }

      new_data <- match_urls |> 
        rlang::set_names() |> 
        purrr::map_dfr(
          \(.x) possibly_scrape_fb_match_summary(
            url = .x,
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
  dplyr::mutate(
    data = purrr::pmap(
      list(
        country,
        gender,
        tier,
        group
      ),
      ~backfill_fb_match_summary(
        country = ..1,
        gender = ..2,
        tier = ..3,
        group = ..4
      )
    )
  )

## could just put this in the function, but i want to check locally before i upload
source(file.path('R', 'piggyback.R'))
local_data |>
  mutate(
    name = sprintf('%s_%s_%s_match_summary', country, gender, tier),
    res = map2(
      data,
      name,
      ~{
        write_worldfootballr_rds_and_csv(
          x = .x,
          name = .y,
          tag = 'fb_match_summary'
        )
      }
    )
  )
