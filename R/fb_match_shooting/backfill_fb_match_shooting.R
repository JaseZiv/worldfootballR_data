library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)

PARENT_DATA_DIR <- file.path('data', 'fb_match_shooting')
SUB_DATA_DIR <- file.path(PARENT_DATA_DIR, 'match_shooting')
dir.create(PARENT_DATA_DIR, showWarnings = FALSE)
dir.create(SUB_DATA_DIR, showWarnings = FALSE)

source(file.path('R', 'fb_match_shooting', 'shared_fb_match_shooting.R'))

scrape_fb_match_shooting <- function(url, data_dir, overwrite = FALSE) {
  rds_path <- file.path(data_dir, sprintf('%s.rds', basename(url)))
  if (!dir.exists(dirname(rds_path))) { dir.create(dirname(rds_path), showWarnings = FALSE, recursive = TRUE) }
  
  suffix <- sprintf('for `url = "%s"`.', url)
  if (file.exists(rds_path) & !overwrite) {
    return(readr::read_rds(rds_path))
  }
  message(sprintf('Scraping data %s', suffix))
  res <- worldfootballR::fb_match_shooting(url)
  readr::write_rds(res, rds_path)
  res
}

possibly_scrape_fb_match_shooting <- possibly(
  scrape_fb_match_shooting, 
  otherwise = tibble::tibble(),
  quiet = FALSE
)

slowly_possibly_scrape_fb_match_shooting <- purrr::slowly(
  possibly_scrape_fb_match_shooting, 
  rate = purrr::rate_delay(pause = 5),
  quiet = FALSE
)

backfill_fb_match_shooting <- function(
    country = 'ENG',
    gender = 'M', 
    tier = '1st', 
    group = 'big5',
    season_end_years = 2025
) {
  
  rds_path <- file.path(PARENT_DATA_DIR, sprintf('%s_%s_%s_match_shooting.rds', country, gender, tier))
  message(sprintf('Updating %s.', rds_path))
  
  if (is.null(season_end_years)) {
    first_season_end_year <- ifelse(
      group == 'big5',
      2018,
      2019
    )
    
    last_season_end_year <- lubridate::year(Sys.Date()) + 1L
    season_end_years <- first_season_end_year:last_season_end_year
  } else {
    last_season_end_year <- max(season_end_years)
  }
  
  res <- purrr::map_dfr(
    season_end_years,
    function(season_end_year) {
      
      season_path <- file.path(SUB_DATA_DIR, country, gender, tier, paste0(season_end_year, '.rds'))
      # if (season_end_year < last_season_end_year & file.exists(season_path)) {
      #   return(readRDS(season_path))
      # }
      if (file.exists(season_path)) {
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
          sprintf('No match URLs for `country = "%s"`, `gender = "%s"`, `tier = "%s"`, `season_end_year = %s`.', country, gender, tier, season_end_year)
        )
        return(tibble::tibble())
      }
      
      new_data <- match_urls |> 
        rlang::set_names() |> 
        purrr::map_dfr(
          \(.x) slowly_possibly_scrape_fb_match_shooting(
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
  # dplyr::filter(
  #   (
  #     # country == 'ENG' &
  #     group != 'big5' &
  #     gender == 'M' &
  #     tier == '1st'
  #   )
  # ) |>
  dplyr::mutate(
    data = purrr::pmap(
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
        group = ..4,
        season_end_years = NULL
      )
    )
  )


## could just put this in the function, but i want to check locally before i upload
# source(file.path('R', 'piggyback.R'))
# local_data |> 
#   mutate(
#     name = sprintf('%s_%s_%s_match_shooting', country, gender, tier),
#     res = map2(
#       data,
#       name,
#       ~{
#         write_worldfootballr_rds_and_csv(
#           x = .x,
#           name = .y,
#           tag = 'fb_match_shooting'
#         )
#       }
#     )
#   )
# 
