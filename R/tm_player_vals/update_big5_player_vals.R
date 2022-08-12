library(worldfootballR)
library(tidyverse)
library(here)
library(rvest)
library(xml2)


existing <- readRDS(here("data", "tm_player_vals", "big5_player_vals.rds"))

scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")

# need to get the latest season available for the big 5 on transfermarkt (using the EPL as the proxy here)
epl_url <- "https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1"
pg <- read_html(epl_url)
max_season <- pg %>% html_nodes(".chzn-select option") %>% html_attr("value") %>% purrr::pluck(1) %>% as.numeric()


# if the latest season is the same as the last season we currently have, update just that season
if(max(existing$season_start_year, na.rm = T) == max_season) {
  
  print(paste0("Scraping data to update current season (", max_season, ")"))
  
  update_season <- tm_player_market_values(country_name = c("England", "Spain", "France", "Italy", "Germany"),
                                           start_year = max_season)
  
  existing_except_new <- existing %>% 
    filter(season_start_year != max_season)
  
  new_df <- bind_rows(
    existing_except_new,
    update_season
  )
  
  # if the latest season on the site is ahead of the latest data we have stores, then append the new data
} else if(max(existing$season_start_year, na.rm = T) > max_season) {
  
  print(paste0("Scraping data to get new season (", max_season, ")"))
  
  update_season <- tm_player_market_values(country_name = c("England", "Spain", "France", "Italy", "Germany"),
                                           start_year = max_season)
  
  new_df <- bind_rows(
    existing,
    update_season
  )
  # otherwise, error, because we don't want to overwrite a season aleady scraped
} else {
  stop(paste0("There is an error and this process might incorrectly overwrite existing data as the latest season available at ",
              epl_url, " is less than the last season data extracted for, which is the season starting ", 
              max(existing$season_start_year)))
}

# then if a new df has been created, then write it to file
if(nrow(new_df) > 0) {
  attr(new_df, "scrape_timestamp") <- scrape_time_utc
  saveRDS(new_df, here("data", "tm_player_vals", "big5_player_vals.rds"))
}



