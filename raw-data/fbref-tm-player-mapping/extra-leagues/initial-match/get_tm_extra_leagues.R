library(worldfootballR)
library(tidyverse)
library(here)




valuations <- tm_player_market_values(country_name = c("Netherlands", "Portugal", "Brazil", "Mexico"),
                                       start_year = c(2018:2022))

saveRDS(valuations, "tm_players_extra_tier1.rds")


#

champ_valuations <- tm_player_market_values(country_name = "", start_year = c(2018:2022), league_url = "https://www.transfermarkt.com/championship/startseite/wettbewerb/GB2")

saveRDS(champ_valuations, "tm_players_championship.rds")
