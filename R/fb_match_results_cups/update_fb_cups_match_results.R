library(worldfootballR)
library(tidyverse)
library(lubridate)
library(janitor)
library(here)


seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

# the below cups are one off matches so we don't need scores and fixtures for these:
exclusion_cups <- c("UEFA Super Cup", "FA Community Shield", "Supercopa de España", "Trophée des Champions", "DFL-Supercup", "Supercoppa Italiana")

latest_cup_seasons <- seasons %>%
  # filtering out things that aren't domestic leagues:
  filter(!stringr::str_detect(.data$competition_type, "Leagues"),
         # and also the single match type cup games:
         !.data$competition_name %in% exclusion_cups) %>% 
  group_by(competition_name) %>% slice_max(season_end_year) %>% 
  distinct()

latest_cup_seasons <- latest_cup_seasons %>% 
  mutate(completed_new = 
           case_when(
             competition_type == "National Team Qualification" & season_end_year >= lubridate::year(lubridate::today()) ~ FALSE,
             is.na(is_completed) ~ FALSE,
             TRUE ~ is_completed
           )
         )



cups_to_get <- latest_cup_seasons %>%
  # filtering out things that aren't domestic leagues:
  dplyr::filter(!completed_new) %>% 
  pull(competition_name) %>% unique()


#=======================================================================================
# Update Match Results ----------------------------------------------------
#=======================================================================================

update_fb_comp_match_results <- function(each_comp) {
  
  print(paste0("Getting Competition: ", each_comp))
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  
  f_name <- janitor::make_clean_names(each_comp)
  
  existing_df <- tryCatch(readRDS(here("data", "match_results_cups", paste0(f_name, "_match_results.rds"))), error = function(e) data.frame())
  
  # we could scrape every leage for the most recent, but if the season has finished, what's the point?
  # The below logic will look to get any games where there are missing scores (we make the assumption that these are not yet played) 
  # and if the date of these games is earlier than the scraping date, then scrape the results
  # df2 <- existing_df %>% filter(Season_End_Year == max(existing_df$Season_End_Year))
  # date_not_collected <- df2 %>% filter(is.na(HomeGoals) & is.na(AwayGoals)) %>% arrange(Date) %>% pull(Date) %>% min()
  
  # if(date_not_collected < Sys.Date()) {
  
  fixture_urls <- latest_cup_seasons %>% filter(competition_name == each_comp) %>% pull(fixtures_url)
  # get the updated data
  new_df <- tryCatch(fixture_urls %>% purrr::map_df(worldfootballR::.get_each_season_results), error = function(e) data.frame())
  
  if(nrow(new_df) != 0) {
    
    new_df_full <- latest_cup_seasons %>% filter(competition_name == each_comp) %>%
      dplyr::select(Competition_Name=.data$competition_name, Gender=.data$gender, Country=.data$country, Season_End_Year=.data$season_end_year, Tier=.data$tier, .data$seasons_urls, .data$fixtures_url) %>%
      dplyr::right_join(new_df, by = c("fixtures_url" = "fixture_url")) %>%
      dplyr::select(-.data$seasons_urls, -.data$fixtures_url) %>%
      dplyr::mutate(Date = lubridate::ymd(.data$Date)) %>%
      dplyr::arrange(.data$Country, .data$Competition_Name, .data$Gender, .data$Season_End_Year, as.numeric(.data$Wk), .data$Date, .data$Time) %>% dplyr::distinct(.keep_all = T)
    
    if(nrow(existing_df) != 0) {
      existing_df <- existing_df %>% 
        anti_join(new_df_full, by = c("Gender", "Season_End_Year", "Tier"))
      
      new_df_full <- bind_rows(existing_df, new_df_full) %>%
        dplyr::arrange(.data$Country, .data$Competition_Name, .data$Gender, .data$Season_End_Year, .data$Date, .data$Time, as.numeric(.data$Wk)) %>% dplyr::distinct(.keep_all = T)
    }
    
    attr(new_df_full, "scrape_timestamp") <- scrape_time_utc
    
    saveRDS(new_df_full, here("data", "match_results_cups", paste0(f_name, "_match_results.rds")))
    
  }
  
  # }
  
  
}


# update data:
cups_to_get %>% purrr::map(update_fb_comp_match_results)

# scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
# saveRDS(scrape_time_utc, here("data", "match_results", "scrape_time_match_results.rds"))


