library(worldfootballR)
library(tidyverse)
library(here)

source("R/piggyback.R")

seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)


latest_seasons <- seasons %>%
  # filtering out things that aren't domestic leagues:
  dplyr::filter(stringr::str_detect(.data[["competition_type"]], "Leagues"),
                tier != "",
                !is.na(.data[["country"]])) %>% 
  filter(!is.na(country), country != "") %>% 
  group_by(country) %>% slice_max(season_end_year) %>% 
  distinct()



countries_to_get <- latest_seasons %>%
  # filtering out things that aren't domestic leagues:
  dplyr::filter(stringr::str_detect(.data[["competition_type"]], "Leagues"),
                tier != "",
                !is.na(.data[["country"]])) %>% 
  filter(!is.na(country), country != "") %>%
  # also want to keep only seasons that are not yet completed
  filter(!is_completed) %>% pull(country) %>% unique()


#=======================================================================================
# Update Match Results ----------------------------------------------------
#=======================================================================================

update_fb_match_results <- function(each_country) {
  
  print(paste0("Getting Country: ", each_country))
  scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
  
  # dat_url <- paste0("https://github.com/JaseZiv/worldfootballR_data/blob/master/data/match_results/", each_country, "_match_results.rds?raw=true")
  # 
  # existing_df <- .file_reader(dat_url)
  
  existing_df <- read_worldfootballr_rds(name=paste0(each_country, "_match_results"), tag = "match_results")
  # existing_df <- tryCatch(readRDS(here("data", "match_results", paste0(each_country, "_match_results.rds"))), error = function(e) data.frame())
  
  # we could scrape every leage for the most recent, but if the season has finished, what's the point?
  # The below logic will look to get any games where there are missing scores (we make the assumption that these are not yet played) 
  # and if the date of these games is earlier than the scraping date, then scrape the results
  # df2 <- existing_df %>% filter(Season_End_Year == max(existing_df$Season_End_Year))
  # date_not_collected <- df2 %>% filter(is.na(HomeGoals) & is.na(AwayGoals)) %>% arrange(Date) %>% pull(Date) %>% min()
  
  # if(date_not_collected < Sys.Date()) {
    
    fixture_urls <- latest_seasons %>% filter(country == each_country) %>% pull(fixtures_url)
    # get the updated data
    new_df <- tryCatch(fixture_urls %>% purrr::map_df(worldfootballR::.get_each_season_results), error = function(e) data.frame())
    
    if(nrow(new_df) != 0) {
      
      new_df_full <- latest_seasons %>% filter(country == each_country) %>%
        dplyr::select(Competition_Name=.data[["competition_name"]], Gender=.data[["gender"]], Country=.data[["country"]], Season_End_Year=.data[["season_end_year"]], Tier=.data[["tier"]], .data[["seasons_urls"]], .data[["fixtures_url"]]) %>%
        dplyr::right_join(new_df, by = c("fixtures_url" = "fixture_url")) %>%
        dplyr::select(-.data[["seasons_urls"]], -.data[["fixtures_url"]]) %>%
        dplyr::mutate(Date = lubridate::ymd(.data[["Date"]])) %>%
        dplyr::arrange(.data[["Country"]], .data[["Competition_Name"]], .data[["Gender"]], .data[["Season_End_Year"]], as.numeric(.data[["Wk"]]), .data[["Date"]], .data[["Time"]]) %>% dplyr::distinct(.keep_all = T)
      
      if(nrow(existing_df) != 0) {
        existing_df <- existing_df %>% 
          anti_join(new_df_full, by = c("Gender", "Season_End_Year", "Tier"))
        
        new_df_full <- bind_rows(existing_df, new_df_full) %>%
          dplyr::arrange(.data[["Country"]], .data[["Competition_Name"]], .data[["Gender"]], .data[["Season_End_Year"]], .data[["Date"]], .data[["Time"]], as.numeric(.data[["Wk"]])) %>% dplyr::distinct(.keep_all = T)
      }
      
      attr(new_df_full, "scrape_timestamp") <- scrape_time_utc
      
      write_worldfootballr(x=new_df_full, name = paste0(each_country, "_match_results"), tag = "match_results", ext = "rds")
      # saveRDS(new_df_full, here("data", "match_results", paste0(each_country, "_match_results.rds")))
      
    }
    
  # }
  
  
}


# update data:
countries_to_get %>% purrr::map(update_fb_match_results)

# scrape_time_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
# saveRDS(scrape_time_utc, here("data", "match_results", "scrape_time_match_results.rds"))


