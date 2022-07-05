library(worldfootballR)
library(tidyverse)
library(here)

# set the working directory to make reading and writing easier
setwd(here("data", "understat_shots"))

# valid league names for scraping data
leagues <- c("EPL", "La_liga", "Bundesliga", "Serie_A", "Ligue_1", "RFPL")


for(each_league in leagues) {
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  
  # first we want to get the current season:
  main_url <- "https://understat.com/"
  page_url <- paste0(main_url, "league/", each_league)
  page <-  tryCatch( xml2::read_html(page_url), error = function(e) NA)
  
  season_element <- page %>% rvest::html_nodes(xpath = '//*[@name="season"]') %>%
    rvest::html_nodes("option")
  season <- season_element %>% rvest::html_attr("value") %>% as.numeric() %>% max(na.rm = T)
  
  # also need to read in the existing shot dat file to see which games have not yet been collected:
  # to do this, we need to clean the valid league names to match the file structure
  league_name_clean <- janitor::make_clean_names(each_league)
  # then read in data
  f <- readRDS(paste0(league_name_clean, "_shot_data.rds"))
  
  # also need to read in the match data to get all match IDs, to then compare wheich matches have been played (and will then have shot data)
  match_data <- worldfootballR::understat_league_match_results(league = each_league, season_start_year = season)
  match_data <- match_data %>% filter(isResult == TRUE)
  
  # only want to keep those match IDs for which we don't have shot data for
  missing_ids <- match_data$match_id[!match_data$match_id %in% f$match_id]
  
  # then, if there are any matches where we don;t already have shot data, go and get them
  if(length(missing_ids) > 0) {
    match_urls <- paste0("https://understat.com/match/", missing_ids)
    
    shots <- match_urls %>% purrr::map_df(worldfootballR::understat_match_shots)
    
    # column names were slightly different prior to the 2021/2022 season - we want to keep these consistent
    if(any(grepl("last_action", names(shots)))) {
      shots <- shots %>% 
        rename(X=x, Y=y, xG=x_g, shotType=shot_type, lastAction=last_action)
    }
    # join them all together
    f <- bind_rows(f, shots)
  }
  
  # now write the file again, regardless of whether there was new data. Will also freshly timestamp the rds
  attr(f, "scrape_timestamp") <- scrape_time_utc
  saveRDS(f, paste0(league_name_clean, "_shot_data.rds"))
}
