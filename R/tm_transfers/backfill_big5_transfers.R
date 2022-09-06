library(worldfootballR)
library(dplyr)
library(here)


countries <- c("England", "Italy", "Germany", "Spain", "France")
all_transfers <- data.frame()

for(each_country in countries) {
  
  each_season_df <- data.frame()
  
  for(i in c(2010:2022)) {
    print(paste0("Scraping country: ", each_country, " for season: ", i))
    urls <- tm_league_team_urls(country_name = each_country, start_year = i)
    season_transfers <- tm_team_transfers(urls)
    each_season_df <- rbind(each_season_df, season_transfers)
  }
  
  all_transfers <- rbind(all_transfers, each_season_df)
  
}



# because the initial scrape was conducted Sep 2022, the current leagues were assigned to teams, but what we want is the relevant 
# league we wanted for the season scraped. Additionally, there are two teams who no longer exist, so these need to be mapped.
# Will manually coerce these here:

all_transfers <- all_transfers %>% 
  dplyr::mutate(
    country = 
      dplyr::case_when(
        team_name == "Athlétic Club Arlésien" ~ "France",
        team_name == "Chievo Verona" ~ "Italy",
        TRUE ~ country
      ),
    league = 
      dplyr::case_when(
        country == "England" ~ "Premier League",
        country == "France" ~ "Ligue 1",
        country == "Germany" ~ "Bundesliga",
        country == "Italy" ~ "Serie A",
        country == "Spain" ~ "LaLiga"
      )
  )


saveRDS(all_transfers, here::here("data", "tm_transfers", "big_5_transfers.rds"))