library(worldfootballR)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(stringi)
library(rvest)
library(purrr)
library(xml2)
library(janitor)

matches_data_dir <- file.path("data", "understat_matches")
matches_subdata_dir <- file.path(matches_data_dir, "league_seasons")
shots_data_dir <- file.path("data", "understat_match_shots")
shots_subdata_dir <- file.path(shots_data_dir, "matches")
dir.create(matches_data_dir, showWarnings = FALSE)
dir.create(matches_subdata_dir, showWarnings = FALSE)
dir.create(shots_data_dir, showWarnings = FALSE)
dir.create(shots_subdata_dir, showWarnings = FALSE)

leagues <- c("La_liga", "EPL", "Bundesliga", "Serie_A", "Ligue_1", "RFPL")
params <- crossing(
  league = leagues,
  season = 2017L:2022L
)

# Reference: https://gist.github.com/Torvaney/42cd82addb3ba2c4f33ec3247e66889c
.extract_json <- function(html, varname) {
  html |>
    rvest::html_elements("script") |>
    rvest::html_text(trim = TRUE) |>
    keep(str_detect, varname) |>
    str_extract("(?<=JSON.parse\\(').*(?='\\))") |>
    stringi::stri_unescape_unicode() |>
    jsonlite::fromJSON()
}

.unnest_df <- function(df, name) {
  if (!is.data.frame(df)) {
    return(tibble(!!name := df))
  }
  colnames(df) <- paste0(name, "_", colnames(df))
  df
}

options(readr.num_columns = 0)
.get_matches <- function(league, season) {
  sprintf("https://understat.com/league/%s/%s", toupper(league), season) |>
    xml2::read_html() |>
    .extract_json("datesData") |>
    imap_dfc(.unnest_df) |>
    rename(match_id = id) |>
    type_convert()
}

get_matches <- function(league, season, overwrite = FALSE) {
  league_name <- janitor::make_clean_names(league)
  rds_path <- file.path(matches_subdata_dir, sprintf("%s_%s_matches.rds", league_name, season)) 
  suffix <- sprintf("for `league = %s`, `season = %s`.", league, season)
  if(file.exists(rds_path) & !overwrite) {
    message(sprintf("Returning pre-saved data %s", suffix))
    return(read_rds(rds_path))
  }
  message(sprintf("Scraping data %s", suffix))
  res <- .get_matches(league, season)
  write_rds(res, rds_path)
  res
}

matches <- params |>
  mutate(data = map2(league, season, get_matches)) |>
  unnest(data)

scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
write_league_matches <- function(league, overwrite = FALSE) {
  
  league_name <- janitor::make_clean_names(league)
  rds_path <- file.path(matches_data_dir, sprintf("%s_matches.rds", league_name))
  path_exists <- file.exists(rds_path)
  csv_path <- file.path(matches_data_dir, sprintf("%s_matches.csv", league_name))

  if (isTRUE(path_exists)) {
    existing_matches <- read_rds(rds_path)
    existing_match_ids <- unique(existing_matches$match_id)
    matches <- matches |> 
      filter(league == !!league, !(match_id %in% existing_match_ids))
  }
  
  if (nrow(matches) == 0) {
    message(sprintf("Not updating data for `league = %s`.", league))
    if (isTRUE(path_exists)) {
      return(existing_matches)
    } else {
      return(data.frame())
    }
  }
  
  completed_matches <- matches |> 
    filter(isResult)

  if (nrow(completed_matches) == 0) {
    message(sprintf("Not updating data for `league = %s`. Bad matches: %s", league, nrow(matches)))
    if (isTRUE(path_exists)) {
      return(existing_matches)
    } else {
      return(data.frame())
    }
  }
  
  if (isTRUE(path_exists)) {
    matches <- bind_rows(
      existing_matches,
      matches
    )
  }
  
  attr(matches, "scrape_timestamp") <- scrape_time_utc
  write_rds(matches, rds_path)
  write_csv(matches, csv_path, na = "")
  matches
}

leagues |> 
  walk(write_league_matches)

get_shots <- function(match_id, overwrite = FALSE) {
  rds_path <- file.path(shots_subdata_dir, sprintf("%s.rds", match_id))
  suffix <- sprintf("for `match_id = %s`.", match_id)
  if(file.exists(rds_path) & !overwrite) {
    message(sprintf("Returning pre-saved data %s", suffix))
    return(read_rds(rds_path))
  }
  Sys.sleep(0.5)
  message(sprintf("Scraping data %s", suffix))
  res <- understat_match_shots(sprintf("https://understat.com/match/%s", match_id))
  write_rds(res, rds_path)
  res
}
possibly_get_shots <- possibly(get_shots, otherwise = data.frame(), quiet = FALSE)

scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
scrape_league_shots <- function(league, overwrite = FALSE) {
  
  league_name <- janitor::make_clean_names(league)
  rds_path <- file.path(matches_data_dir, sprintf("%s_shots.rds", league_name))
  path_exists <- file.exists(rds_path)
  csv_path <- file.path(matches_data_dir, sprintf("%s_shots.csv", league_name))
  
  if (isTRUE(path_exists)) {
    existing_shots <- read_rds(rds_path)
    existing_match_ids <- unique(existing_shots$match_id)
    matches <- matches |> 
      filter(league == !!league, !(match_id %in% existing_match_ids))
  }
  
  if (nrow(matches) == 0) {
    message(sprintf("Not updating data for `league = %s`.", league))
    if (isTRUE(path_exists)) {
      return(existing_shots)
    } else {
      return(data.frame())
    }
  }
  
  shots <- matches |> 
    filter(isResult) |> 
    pull(match_id) |> 
    map_dfr(possibly_get_shots)

  if (nrow(completed_matches) == 0) {
    message(sprintf("Not updating data for `league = %s`. Bad matches: %s", league, nrow(shots)))
    if (isTRUE(path_exists)) {
      return(existing_shots)
    } else {
      return(data.frame())
    }
  }
  
  if (isTRUE(path_exists)) {
    shots <- bind_rows(
      existing_shots,
      shots
    )
  }
  
  attr(shots, "scrape_timestamp") <- scrape_time_utc
  write_rds(shots, rds_path)
  write_csv(shots, csv_path, na = "")
  shots
}

leagues |> 
  walk(scrape_league_shots)

