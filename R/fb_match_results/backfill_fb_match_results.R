library(worldfootballR)
library(tidyverse)
library(here)

#============================================================================
# Set up historic results -------------------------------------------------

# seasons <- c(1989:2021)
# 
# historic_big_5_results <- data.frame()
# tier_collect <- "1st"
# 
# for(each_season in seasons) {
#   Sys.sleep(5)
#   df <- get_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"),
#                           gender = "M", season_end_year = each_season, tier = tier_collect)
#   df <- df %>% 
#     dplyr::mutate(tier = tier_collect)
#   
#   historic_big_5_results <- dplyr::bind_rows(historic_big_5_results, df)
# }
# 
# saveRDS(historic_big_5_results, here("data", "match_results", "historic_big_5_results_to_2021.rds"))



#============================================================================
# Set up historic results -------------------------------------------------

# .get_each_season_results <- function(fixture_url) {
#   Sys.sleep(3)
#   main_url <- "https://fbref.com"
#   
#   # pb$tick()
#   
#   # fixtures_page <- fixture_url %>% httr::GET(., timeout(30)) %>% xml2::read_html()
#   fixtures_page <- xml2::read_html(fixture_url)
#   
#   season_name <- fixtures_page %>% rvest::html_nodes("h2 span") %>% rvest::html_text() %>% .[1]
#   
#   tab_holder <- fixtures_page %>%
#     rvest::html_node(".stats_table tbody") %>% rvest::html_nodes("tr")
#   
#   spacer_idx <- !grepl("spacer partial", xml2::xml_attrs(tab_holder))
#   
#   tab_holder <- tab_holder[spacer_idx]
#   
#   idx_rm <- grep("thead", xml2::xml_attrs(tab_holder))
#   
#   if(length(idx_rm) != 0) {
#     tab_holder <- tab_holder[-idx_rm]
#   }
#   
#   season_summary <- tryCatch(fixtures_page %>%
#                                rvest::html_table() %>% .[1] %>% data.frame(), error = function(e) data.frame())
#   
#   # error handling - The first available Iranian season has something weird in the HTML, meaning the above code wont work
#   # (https://fbref.com/en/comps/64/741/schedule/2014-2015-Persian-Gulf-Pro-League-Scores-and-Fixtures)
#   if(nrow(season_summary) == 0) {
#     season_summary <- fixtures_page %>% rvest::html_node(".stats_table") %>%
#       rvest::html_table() %>% data.frame()
#   }
#   
#   season_summary <- season_summary[spacer_idx,]
#   
#   if(length(idx_rm) != 0) {
#     season_summary <- season_summary[-idx_rm,]
#   }
#   
#   
#   # tab_holder <- fixtures_page %>%
#   #   rvest::html_nodes(".stats_table tbody tr")
#   
#   
#   get_url <- function(tab_element) {
#     a <- tab_element %>% rvest::html_node(xpath='.//*[@data-stat="match_report"]//a') %>% rvest::html_attr("href")
#     if(is.na(a) || length(a) == 0) {
#       a <- NA_character_
#     } else {
#       a <-  paste0(main_url, a)
#     }
#     
#     return(a)
#   }
#   
#   match_urls <- purrr::map_chr(tab_holder, get_url)
#   # match_urls <- match_urls[!duplicated(match_urls, incomparables = NA)]
#   
#   
#   suppressWarnings(
#     season_summary <- season_summary %>%
#       dplyr::filter(is.na(.data$Time) | .data$Time != "Time") %>%
#       dplyr::mutate(Score = iconv(.data$Score, 'utf-8', 'ascii', sub=' ') %>% stringr::str_squish()) %>%
#       tidyr::separate(.data$Score, into = c("HomeGoals", "AwayGoals"), sep = " ") %>%
#       dplyr::mutate(HomeGoals = as.numeric(.data$HomeGoals),
#                     AwayGoals = as.numeric(.data$AwayGoals),
#                     Attendance = as.numeric(gsub(",", "", .data$Attendance)))
#   )
#   
#   season_summary <- season_summary %>%
#     dplyr::mutate(HomeGoals = ifelse(is.na(.data$HomeGoals) & !is.na(.data$AwayGoals), .data$AwayGoals, .data$HomeGoals),
#                   AwayGoals = ifelse(is.na(.data$AwayGoals) & !is.na(.data$HomeGoals), .data$HomeGoals, .data$AwayGoals))
#   
#   season_summary <- cbind(fixture_url, season_summary)
#   
#   if(!any(stringr::str_detect(names(season_summary), "Round"))) {
#     Round <- rep(NA, nrow(season_summary))
#     season_summary <- cbind(Round, season_summary)
#   }
#   
#   if(!any(stringr::str_detect(names(season_summary), "Wk"))) {
#     Wk <- rep(NA, nrow(season_summary))
#     season_summary <- cbind(Wk, season_summary)
#   }
#   
#   if(any(stringr::str_detect(names(season_summary), "xG"))) {
#     season_summary <- season_summary %>%
#       dplyr::select(.data$fixture_url, Round, .data$Wk, .data$Day, .data$Date, .data$Time, .data$Home, .data$HomeGoals, Home_xG=.data$xG, .data$Away, .data$AwayGoals, Away_xG=.data$xG.1, .data$Attendance, .data$Venue, .data$Referee, .data$Notes) %>% 
#       dplyr::mutate(Home_xG = as.numeric(.data$Home_xG),
#                     Away_xG = as.numeric(.data$Away_xG))
#   } else {
#     season_summary <- season_summary %>%
#       dplyr::select(.data$fixture_url, Round, .data$Wk, .data$Day, .data$Date, .data$Time, .data$Home, .data$HomeGoals, .data$Away, .data$AwayGoals, .data$Attendance, .data$Venue, .data$Referee, .data$Notes)
#   }
#   
#   season_summary <- season_summary %>%
#     dplyr::mutate(Wk = as.character(.data$Wk)) %>% 
#     dplyr::mutate(MatchURL = match_urls)
#   
#   return(season_summary)
# }



source("R/piggyback.R")


backfill_historical_results <- function(country_collect) {
  
  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)
  
  
  fixtures_df <- seasons %>%
    # filtering out things that aren't domestic leagues:
    dplyr::filter(stringr::str_detect(.data$competition_type, "Leagues"),
                  tier != "",
                  !is.na(.data$country)) %>% 
    # get seasons that are only for the country selected
    dplyr::filter(country %in% country_collect,
                  !is.na(.data$fixtures_url)) %>%
    dplyr::arrange(desc(.data$season_end_year))
  
  fixtures_urls <- fixtures_df %>% 
    dplyr::pull(.data$fixtures_url) %>% unique()
  
  # pb <- progress::progress_bar$new(total = length(fixtures_urls))
  
  # all_results <- fixtures_urls %>%
  #   purrr::map_df(.get_each_season_results)
  
  all_results <- data.frame()
  for(each_fixture in 1:length(fixtures_urls)) {
    print(paste0("Scraping URL ", each_fixture, " of ", length(fixtures_urls)))
    df <- tryCatch(worldfootballR::.get_each_season_results(fixture_url = fixtures_urls[each_fixture], time_pause = 4), error = function(e) data.frame())
    
    all_results <- bind_rows(all_results, df)
  }
  
  all_results <- fixtures_df %>%
    dplyr::select(Competition_Name=.data$competition_name, Gender=.data$gender, Country=.data$country, Season_End_Year=.data$season_end_year, Tier=.data$tier, .data$seasons_urls, .data$fixtures_url) %>%
    dplyr::right_join(all_results, by = c("fixtures_url" = "fixture_url")) %>%
    dplyr::select(-.data$seasons_urls, -.data$fixtures_url) %>%
    dplyr::mutate(Date = lubridate::ymd(.data$Date)) %>%
    dplyr::arrange(.data$Country, .data$Competition_Name, .data$Gender, .data$Season_End_Year, .data$Wk, .data$Date, .data$Time) %>% dplyr::distinct(.keep_all = T)
  
  # return(all_results)
  
  # saveRDS(all_results, here("data", "match_results", paste0(country_collect, "_match_results.rds")))
  write_worldfootballr(x=all_results, name = paste0(country_collect, "_match_results"), tag = "match_results", ext = "rds")
}




seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)


countries_to_get <- seasons %>%
  # filtering out things that aren't domestic leagues:
  dplyr::filter(stringr::str_detect(.data$competition_type, "Leagues"),
                tier != "",
                !is.na(.data$country)) %>% 
  filter(!is.na(country), country != "") %>% pull(country) %>% unique()

# exclude_countries <- c("ENG", "BEL", "ARG", "AUS", "AUT")
# countries_new <- countries_to_get[!countries_to_get %in% exclude_countries]

# countries_to_get <- seasons %>% filter(!is.na(country), country != "") %>% pull(country) %>% unique()


for(each_country in countries_to_get){
  print(paste("Scraping", each_country))
  backfill_historical_results(each_country)
}




