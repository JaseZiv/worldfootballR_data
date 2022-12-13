library(worldfootballR)
library(tidyverse)
library(here)

source("R/piggyback.R")

backfill_historical_comp_results <- function(competition_collect) {
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  
  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)
  
  
  fixtures_df <- seasons %>%
    # get only things that aren't domestic leagues:
    dplyr::filter(!stringr::str_detect(.data[["competition_type"]], "Leagues")) %>% 
    # get seasons that are only for the competition selected
    dplyr::filter(competition_name %in% competition_collect,
                  !is.na(.data[["season_end_year"]])) %>%
    dplyr::arrange(desc(.data[["season_end_year"]]))
  
  fixtures_urls <- fixtures_df %>% 
    dplyr::pull(.data[["fixtures_url"]]) %>% unique()
  
  
  all_results <- data.frame()
  for(each_fixture in 1:length(fixtures_urls)) {
    print(paste0("Scraping URL ", each_fixture, " of ", length(fixtures_urls)))
    df <- worldfootballR::.get_each_season_results(fixture_url = fixtures_urls[each_fixture], time_pause = runif(1, 4, 6))
    
    all_results <- bind_rows(all_results, df)
  }
  
  all_results <- fixtures_df %>%
    dplyr::select(Competition_Name=.data[["competition_name"]], Gender=.data[["gender"]], Country=.data[["country"]], Season_End_Year=.data[["season_end_year"]], Tier=.data[["tier"]], .data[["seasons_urls"]], .data[["fixtures_url"]]) %>%
    dplyr::right_join(all_results, by = c("fixtures_url" = "fixture_url")) %>%
    dplyr::select(-.data[["seasons_urls"]], -.data[["fixtures_url"]]) %>%
    dplyr::mutate(Date = lubridate::ymd(.data[["Date"]])) %>%
    dplyr::arrange(.data[["Country"]], .data[["Competition_Name"]], .data[["Gender"]], .data[["Season_End_Year"]], .data[["Wk"]], .data[["Date"]], .data[["Time"]]) %>% dplyr::distinct(.keep_all = T)
  
  # return(all_results)
  # clean names for files - will need to repeat this step for loading functions to convert the text users will see
  # as the competition name to this file name structure
  comp_name_file <- janitor::make_clean_names(competition_collect)
  # add the time stamp
  attr(all_results, "scrape_timestamp") <- scrape_time_utc
  
  # saveRDS(all_results, here("data", "match_results_cups", paste0(comp_name_file, "_match_results.rds")))
  write_worldfootballr(x=all_results, name = paste0(comp_name_file, "_match_results"), tag = "match_results_cups", ext = "rds")
}



#==================================================================================================================================================
# Get Data ----------------------------------------------------------------

seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)


cups_to_get <- seasons %>%
  # Getting only things that aren't domestic leagues:
  dplyr::filter(!stringr::str_detect(.data[["competition_type"]], "Leagues"),
                !is.na(.data[["season_end_year"]])) %>% 
  dplyr::pull(competition_name) %>% unique()


# the below cups are one off matches so we don't need scores and fixtures for these:
exclusion_cups <- c("UEFA Super Cup", "FA Community Shield", "Supercopa de España", "Trophée des Champions", "DFL-Supercup", "Supercoppa Italiana")

# filter them out
cups_to_get <- cups_to_get[!cups_to_get %in% exclusion_cups]

# get data for all cups/competitions
for(each_cup in cups_to_get){
  print(paste("Scraping", each_cup))
  backfill_historical_comp_results(each_cup)
}
