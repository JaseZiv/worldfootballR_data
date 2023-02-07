


library(RSelenium)
library(xml2)
library(rvest)
library(tidyverse)


# Set Up Selenium ---------------------------------------------------------

rD <- rsDriver(browser="firefox", port=4445L, verbose=TRUE)
remDr <- rD[["client"]]

# remDr$navigate("https://fbref.com/en/comps/23/2020-2021/playingtime/2020-2021-Eredivisie-Stats")


# function to open page
read_html_selenium <- function (page_url, driver, sleep) {
  
  if (missing(driver)) {
    driver <- remDr
    
  }
  
  if (missing(sleep)) {
    sleep <- 0
  }
  
  
  remDr$navigate(page_url)
  Sys.sleep(1)
  # need to get to the bottom of the page to expose all 36 products per page
  webElem <- remDr$findElement("css", "body")
  Sys.sleep(1)
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(2)
  # webElem <- remDr$findElement("css", ".paginator")
  # webElem$sendKeysToElement(list(key = "end"))
  
  remDr$getPageSource(.) %>%
    .[[1]] %>% .[1] %>% read_html(.)
  
}



# Variables ---------------------------------------------------------------

main_url <- "https://fbref.com"


country_abbr <- c("USA")
gender_M_F <- "M"
season_end_year_num <- c(2019:2023)
comp_tier <- "1st"



# Get Seasons URLs ---------------------------------------------------------

seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

seasons_urls <- seasons %>%
  dplyr::filter(stringr::str_detect(.data[["competition_type"]], "Leagues")) %>%
  dplyr::filter(country %in% country_abbr,
                gender %in% gender_M_F,
                season_end_year %in% season_end_year_num,
                tier %in% comp_tier) %>%
  dplyr::arrange(season_end_year) %>%
  dplyr::pull(seasons_urls) %>% unique()


# championship_seasons_urls <- seasons %>%
#   dplyr::filter(stringr::str_detect(.data[["competition_type"]], "Leagues")) %>%
#   dplyr::filter(country == "ENG",
#                 gender == "M",
#                 season_end_year %in% c(2019:2023),
#                 tier == "2nd") %>%
#   dplyr::arrange(season_end_year) %>%
#   dplyr::pull(seasons_urls) %>% unique()
# 
# 
# seasons_urls <- c(seasons_urls, championship_seasons_urls)



# Scrape FBREF ------------------------------------------------------------
fbref <- data.frame()

for(season_url in seasons_urls) {
  
  print(paste0("scraping season: ", season_url))
  
  start_part <- sub('/[^/]*$', '', season_url)
  end_part <- gsub(".*/", "", season_url)
  
  stat_urls <- paste0(start_part, "/", "playingtime", "/", end_part)
  
  Sys.sleep(5)
  pg <- read_html_selenium(stat_urls)
  
  tab_elem <- pg %>% html_elements("#div_stats_playing_time")
  
  urls <- tab_elem %>%
    rvest::html_nodes("table") %>%
    rvest::html_nodes("tbody") %>%
    rvest::html_nodes("tr") %>% rvest::html_node("td a") %>% rvest::html_attr("href") %>% paste0(main_url, .)
  
  stat_df <- tab_elem %>% html_table() %>% data.frame()
  stat_df <- stat_df[,c(2,3,4,5,7)]
  stat_df_names <- stat_df[1,] %>% as.character()
  stat_df <- stat_df[-1,]
  colnames(stat_df) <- stat_df_names
  
  stat_df$Url <- urls
  stat_df$season_url <- season_url
  
  stat_df <- stat_df %>%
    filter(Nation != "Nation")
  
  stat_df <- stat_df %>%
    left_join(seasons %>% select(season_end_year, competition_name, seasons_urls), by = c("season_url" = "seasons_urls"))
  
  fbref <- bind_rows(fbref, stat_df)
}



setwd("../")
saveRDS(fbref, here::here("raw-data", "fbref-tm-player-mapping", "extra-leagues", "initial-match", "fbref_mls.rds"))


library(worldfootballR)


valuations <- data.frame()

for(i in c(2018:2021)) {
  
  print(paste("scraping year:", i))
  vals <- tm_player_market_values(country_name = c("United States"),
                                  start_year = i)
  
  valuations <- bind_rows(valuations, vals)
}



saveRDS(valuations, here::here("raw-data", "fbref-tm-player-mapping", "extra-leagues", "initial-match", "tm_players_mls.rds"))
