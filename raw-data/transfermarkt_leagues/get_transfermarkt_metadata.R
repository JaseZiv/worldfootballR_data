library(tidyverse)
library(rvest)

main_url <- "https://www.transfermarkt.com"


# Get Competitions --------------------------------------------------------

# need to hard code this - no idea where to go to get the regions:
regions <- c("europa", "asien", "amerika", "afrika")

# region_urls <- paste0("https://www.transfermarkt.com/wettbewerbe/", regions)

all_comps <- data.frame()

for(region in regions) {
  print(paste("Scraping league URLs from the", region, "region"))
  region_url <- paste0("https://www.transfermarkt.com/wettbewerbe/", region)
  Sys.sleep(3)
  comp <- xml2::read_html(region_url)
  
  comp_name <- comp %>% rvest::html_nodes(".inline-table td+ td a") %>% rvest::html_text()
  comp_url <- comp %>% rvest::html_nodes(".inline-table td+ td a") %>% rvest::html_attr("href") %>% paste0(main_url, .)
  
  flags_list <- comp %>% rvest::html_nodes(".hauptlink+ .zentriert")
  country <- c()
  for(i in 1:length(flags_list)) {
    a <- xml2::xml_attrs(xml2::xml_child(flags_list[[i]], 1))[["title"]]
    country <- c(country, a)
  }
  
  comps_df <- cbind(comp_name, region, country, comp_url) %>% data.frame()
  
  all_comps <- rbind(all_comps, comps_df)
}


# Get season URLs ---------------------------------------------------------


all_seasons_df <- data.frame()

for(each_league_url in 1:nrow(all_comps)) {
  print(paste0("scraping league ", each_league_url, " of ", nrow(all_comps)))
  Sys.sleep(4)
  comp_url <- all_comps$comp_url[each_league_url]
  league_page <- xml2::read_html(comp_url)
  
  seasons <- league_page %>% rvest::html_nodes(".chzn-select") %>% rvest::html_nodes("option")
  
  season_start_year <- c()
  for(each_season in seasons) {
    season_start_year <- c(season_start_year, xml2::xml_attrs(each_season)[["value"]])
  }
  
  season_urls <- paste0(comp_url, "/plus/?saison_id=", season_start_year)
  
  league_seasons_df <- cbind(comp_url, season_start_year, season_urls) %>% data.frame()
  
  all_seasons_df <- rbind(all_seasons_df, league_seasons_df)
}

all_data <- all_comps %>% 
  dplyr::left_join(all_seasons_df, by = "comp_url")

all_data <- all_data %>% 
  dplyr::mutate(region = dplyr::case_when(
    region == "europa" ~ "Europe",
    region == "asien" ~ "Asia",
    region == "amerika" ~ "Americas",
    region == "afrika" ~ "Africa"
  ))

write.csv(all_data, here::here("raw-data", "transfermarkt_leagues", "main_comp_seasons.csv"), row.names = F)


