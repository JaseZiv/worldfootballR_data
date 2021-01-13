library(tidyverse)
library(worldfootballR)

.get_tier1_competitions <- function() {
  main_url <- "https://fbref.com"
  # read page to all competitions
  all_comps_url <- xml2::read_html("https://fbref.com/en/comps/")
  # this just gets the Tier 1 club comps - this will need to be modified if more comps are required
  comps <- all_comps_url %>% rvest::html_nodes("#all_comps_1_fa_club_league_senior")
  # get the urls for each competition, then paste fbref url
  competition_urls <- comps %>% rvest::html_node("tbody") %>% rvest::html_nodes("th a") %>% rvest::html_attr("href")
  competition_urls <- paste0(main_url, competition_urls)
  # scrape the table that contains the competitons
  competitions <- comps %>% rvest::html_nodes(".sortable") %>% rvest::html_table() %>% data.frame()
  # add the competition url column
  competitions <- cbind(competitions, competition_urls)
  # remove the two character country code for the flag, and only leave the 3 character code
  competitions$Country <- gsub(".*? ", "", competitions$Country)

  return(competitions)
}


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
    
    # fixtures_url <- xml2::read_html(season_url) %>%
    #   rvest::html_nodes(".hoversmooth") %>%
    #   rvest::html_nodes(".full") %>%
    #   rvest::html_nodes("a") %>%
    #   rvest::html_attr("href") %>% .[grepl("Fixtures", .)] %>% paste0(main_url, .)
    
    get_fixtures <- function(season_url) {
      round(runif(1, 3, 10))
      fixtures_url <- xml2::read_html(season_url) %>%
        rvest::html_nodes(".hoversmooth") %>%
        rvest::html_nodes(".full") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>% .[grepl("Fixtures", .)] %>% paste0(main_url, .)
      
      fixtures_url <- if(grepl("Fixtures", fixtures_url)){
        fixtures_url <- fixtures_url
      } else {
        fixtures_url <- NA
      }
      
      return(fixtures_url)
    }
    
    fixtures_url <- seasons_urls %>% 
      purrr::map_chr(get_fixtures)
    
    all_league_seasons <- cbind(league_url, seasons, season_end_year, seasons_urls, fixtures_url) %>% data.frame()
    
    
    return(all_league_seasons)
  }
  
  all_urls <- league_urls %>% 
    purrr::map_df(get_urls) %>% 
    dplyr::left_join(competitions, ., by = c("competition_urls" = "league_url")) %>% 
    janitor::clean_names()
  
}



all_tier1_season_URLs <- get_league_seasons_url()

write.csv(all_tier1_season_URLs, here::here("raw-data", "league_seasons", "all_tier1_season_URLs.csv"), row.names = F)
