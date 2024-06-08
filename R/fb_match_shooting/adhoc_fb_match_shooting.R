library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)

source(file.path('R', 'piggyback.R'))
source(file.path('R', 'fb_match_shooting', 'shared_fb_match_shooting.R'))

fb_match_shooting_tag <- 'fb_match_shooting'
country <- 'ENG'
gender <- 'M'
tier <- '1st'
name <- sprintf('%s_%s_%s_match_shooting', country, gender, tier)
existing_fb_match_shooting  <- read_worldfootballr_rds(
  name = name, 
  tag = fb_match_shooting_tag
)

scrape_fb_match_shooting <- function(match_url) {
  message(sprintf('Scraping matches for %s.', match_url))
  fb_match_shooting(match_url)
}

## games not including all shots when originally scraped
new_fb_match_shooting <- c(
  c(
    'https://fbref.com/en/matches/070bf86d/Burnley-Newcastle-United-May-4-2024-Premier-League',
    'https://fbref.com/en/matches/91a2da3b/Sheffield-United-Nottingham-Forest-May-4-2024-Premier-League'
  )
) |> 
  set_names() |> 
  map_dfr(scrape_fb_match_shooting, .id = 'MatchURL') |> 
  as_tibble()

matching_matches <- new_fb_match_shooting |> 
  distinct(MatchURL) |> 
  inner_join(
    existing_fb_match_shooting |> 
      distinct(MatchURL, Competition_Name, Gender, Country, Tier, Season_End_Year)
  )

bind_rows(
  existing_fb_match_shooting |> 
    filter(!(MatchURL %in% matching_matches$MatchURL)),
  new_fb_match_shooting |> 
    left_join(matching_matches)
) |> 
  mutate(Tier = '1st') |> ## temp fix
  write_worldfootballr_rds_and_csv(
    name = name,
    tag = fb_match_shooting_tag
  )
  