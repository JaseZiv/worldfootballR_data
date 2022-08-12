library(worldfootballR)
library(tidyverse)
library(here)

Top5_transfers_2010_2020 <- readRDS("~/Documents/Jason/Analysis-of-worldfootballR/Top5_transfers_2010_2020.rds")


countries <- c("England", "Italy", "Germany", "Spain", "France")
all_transfers <- data.frame()

for(each_country in countries) {

  each_season_df <- data.frame()

  for(i in c(2020:2022)) {
    print(paste0("Scraping country: ", each_country, " for season: ", i))
    urls <- tm_league_team_urls(country_name = each_country, start_year = i)
    season_transfers <- tm_team_transfers(urls)
    each_season_df <- rbind(each_season_df, season_transfers)
  }

  all_transfers <- rbind(all_transfers, each_season_df)

}




all_transfers <- all_transfers %>% rename(season=i)

# saveRDS(all_transfers, "Top5_transfers.rds")