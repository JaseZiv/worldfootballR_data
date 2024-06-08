library(worldfootballR)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(rlang)

source(file.path('R', 'piggyback.R'))
source(file.path('R', 'fb_match_shooting', 'shared_fb_match_shooting.R'))

## fix tier ----
fb_match_shooting_tag <- 'fb_match_shooting'
params |>
  filter(group != 'non_domestic') |> 
  mutate(
    data = pmap(
      list(
        country,
        gender,
        tier
      ),
      \(.country, .gender, .tier) {
        name <- sprintf('%s_%s_%s_match_shooting', .country, .gender, .tier)
        message(sprintf('Updating %s.', name))
        existing_match_shooting <- read_worldfootballr_rds(
          name = name, 
          tag = fb_match_shooting_tag
        )
        
        if (all(!is.na(existing_match_shooting$Tier))) {
          return(existing_match_shooting)
        }
        existing_match_shooting |> 
          mutate(Tier = coalesce(Tier, .tier)) |>
          write_worldfootballr_rds_and_csv(
            name = name,
            tag = fb_match_shooting_tag
          )
      }
    )
  )

## fix some incomplete matches ----
fb_match_shooting_tag <- 'fb_match_shooting'
country <- 'ITA'
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
  ## ENG
  # 'https://fbref.com/en/matches/070bf86d/Burnley-Newcastle-United-May-4-2024-Premier-League',
  # 'https://fbref.com/en/matches/91a2da3b/Sheffield-United-Nottingham-Forest-May-4-2024-Premier-League'
  ## FRA
  # 'https://fbref.com/en/matches/9a72e466/Lille-Lyon-May-6-2024-Ligue-1'
  ## GER
  # 'https://fbref.com/en/matches/0c04e055/Hoffenheim-RB-Leipzig-May-3-2024-Bundesliga', 
  # 'https://fbref.com/en/matches/38c2e1c6/Stuttgart-Bayern-Munich-May-4-2024-Bundesliga'
  ## ITA
  'https://fbref.com/en/matches/f8a1cbc3/Derby-della-Madonnina-Milan-Internazionale-April-22-2024-Serie-A'
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
  write_worldfootballr_rds_and_csv(
    name = name,
    tag = fb_match_shooting_tag
  )
