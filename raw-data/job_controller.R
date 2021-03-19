# SCHEDULED SCRAPERS

# load libraries
library(here)

# Scrape League Seasons Data ------------------------------------------------------
source(here::here("raw-data", "league_seasons", "get_league_seasons.R"))

# Scrape Countries Data ---------------------------------------------------
source(here::here("raw-data", "countries_list", "get_countries_list.R"))
