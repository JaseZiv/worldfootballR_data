library(tidyverse)
library(rvest)

# thanks to TanHo for the below reading function
.get_page <- function(url){
  httr::GET(url,httr::user_agent("worldfootballR data package <https://github.com/JaseZiv/worldfootballR>"))  %>%  
    httr::content(as = "text")  %>%  
    rvest::read_html()
}

get_page <- ratelimitr::limit_rate(.get_page, ratelimitr::rate(1,3))


.get_competitions <- function() {
  
  main_url <- "https://fbref.com"
  Sys.sleep(3)
  comps_page <- xml2::read_html("https://fbref.com/en/comps/")
  
  
  tabs <- comps_page %>% html_nodes(".table_wrapper")
  
  full_df <- data.frame()
  
  for(each in tabs) {
    tab <- each %>%
      rvest::html_nodes("table") %>%
      rvest::html_table() %>% data.frame() %>% dplyr::mutate_all(as.character)
    
    if(grepl("1st Tier", each %>% rvest::html_nodes("h2") %>% rvest::html_text())) {
      tab$Tier <- "1st"
    } else if(grepl("2nd Tier", each %>% rvest::html_nodes("h2") %>% rvest::html_text())) {
      tab$Tier <- "2nd"
    } else {
      tab <- each %>%
        rvest::html_nodes("table") %>%
        rvest::html_table() %>% data.frame() %>% dplyr::mutate_all(as.character)
    }
    
    tab$comp_url <- each %>%
      rvest::html_nodes("th a") %>%
      rvest::html_attr("href") %>% paste0(main_url, .)
    
    tab$Competition.Type <- each %>% rvest::html_nodes("h2") %>% rvest::html_text()
    
    if(!any(grepl("Country", names(tab)))) {
      tab$Country <- NA_character_
    } 
    
    full_df <- dplyr::bind_rows(full_df, tab)
    
    full_df <- full_df %>%
      dplyr::select(Competition.Type, Competition.Name, Country, dplyr::everything())
    
    full_df$Country <- gsub(".*? ", "", full_df$Country)
  }
  
  return(full_df)
  
}


get_league_seasons_url <- function() {
  main_url <- "https://fbref.com"
  
  competitions <- .get_competitions()
  
  league_urls <- competitions %>%
    dplyr::pull(.data$comp_url)
  
  get_urls <- function(league_url) {
    # Sys.sleep(runif(1, min=4, max=10))
    print(glue::glue("Scraping season URLs from {league_url}"))
    Sys.sleep(3)
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
    
    # use regex to construct fixtures_urls... this may cause some downstream issues as not all
    # season_urls have fixture URLs (individual matches not on FBref for some leagues historically)
    # but still, the time saving and not being blocked is worth this as error handling in worldfootballR
    # functions should just return zero row data frames
    to_keep <- gsub(".*/", "", seasons_urls) %>% gsub("-Stats", "-Scores-and-Fixtures", .)
    url_stripped <- sub("/[^/]+$", "", seasons_urls)
    url_amend <- paste0(url_stripped, "/schedule/")
    
    fixtures_url <- paste0(url_amend, to_keep)
    
    all_league_seasons <- cbind(league_url, seasons, season_end_year, seasons_urls, fixtures_url) %>% data.frame()
    
    
    return(all_league_seasons)
  }
  
  all_urls <- league_urls %>% 
    purrr::map_df(get_urls) %>% 
    dplyr::left_join(competitions, ., by = c("comp_url" = "league_url")) %>% 
    janitor::clean_names() %>%
    # want to keep the big five leagues combined, but not the individual leages (these would be duplicated if kept in)
    dplyr::mutate(filter_out = dplyr::case_when(
      str_detect(competition_type, "Big 5") & !str_detect(competition_name, "Big 5 ") ~ "Y",
      TRUE ~ "N"
    )) %>% 
    dplyr::filter(filter_out == "N")
  
  return(all_urls)
  
}


all_comp_urls <- get_league_seasons_url()

all_comp_urls <- all_comp_urls %>% 
  dplyr::mutate(tier = ifelse(stringr::str_detect(competition_type, "Youth"), "Youth", tier))

write.csv(all_comp_urls, here::here("raw-data", "all_leages_and_cups", "all_competitions.csv"), row.names = F)

