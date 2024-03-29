# SCHEDULED SCRAPERS

# load libraries
library(here)

# Scrape League Seasons Data ------------------------------------------------------
# source(here::here("raw-data", "league_seasons", "get_league_seasons.R"))

# Scrape Countries Data ---------------------------------------------------
# source(here::here("raw-data", "countries_list", "get_countries_list.R"))



# Scrape All Competition Season’s Data ------------------------------------
source(here::here("raw-data", "all_leages_and_cups", "get_all_comp_seasons.R"))



# Scrape Transfermarkt Data -----------------------------------------------
source(here::here("raw-data", "transfermarkt_leagues", "get_transfermarkt_metadata.R"))


# Scrape Fotmob league data ----------------------------------------------
# source(here::here("raw-data", "fotmob-leagues", "get_fotmob_leagues.R"))

