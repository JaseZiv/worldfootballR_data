library(tidyverse)
library(worldfootballR)



get_league_seasons_url <- function() {
  main_url <- "https://fbref.com"
  
  competitions <- .get_tier1_competitions()
  
  league_urls <- competitions %>%
    dplyr::pull(.data$competition_urls)
  
  get_urls <- function(league_url) {
    print(glue::glue("Scraping season URLs from {league_url}"))
    league_page <- xml2::read_html(league_url)
    
    seasons <- league_page %>%
      rvest::html_nodes("th a") %>%
      rvest::html_text()
    
    season_end_year <- league_page %>%
      rvest::html_nodes("th a") %>%
      rvest::html_text() %>%
      gsub(".*-", "", .)
    
     
    seasons_urls <- league_page %>%
      rvest::html_nodes("th a") %>%
      rvest::html_attr("href") %>%
      paste0(main_url, .)
    
    
    all_league_seasons <- cbind(league_url, seasons, season_end_year, seasons_urls) %>% data.frame()
    
    
    return(all_league_seasons)
  }
  
  all_urls <- league_urls %>% 
    purrr::map_df(get_urls) %>% 
    dplyr::left_join(competitions, ., by = c("competition_urls" = "league_url")) %>% 
    janitor::clean_names()
  
}



all_tier1_season_URLs <- get_league_seasons_url()

write.csv(all_tier1_season_URLs, here::here("raw-data", "league_seasons", "all_tier1_season_URLs.csv"), row.names = F)