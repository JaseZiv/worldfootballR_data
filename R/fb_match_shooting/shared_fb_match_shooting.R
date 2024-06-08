library(dplyr)
params <- bind_rows(
  'big5' = list(
    country = c('ENG', 'ESP', 'FRA', 'GER', 'ITA'),
    tier = '1st',
    gender = 'M',
    non_dom_league_url = NA_character_
  ),
  'other_1st_M' = list(
    country = c('POR', 'NED', 'BRA', 'MEX', 'USA'),
    tier = '1st',
    gender = 'M',
    non_dom_league_url = NA_character_
  ),
  '1st_F' = list(
    country = c('ENG', 'USA', 'ESP'),
    tier = '1st',
    gender = 'F',
    non_dom_league_url = NA_character_
  ),
  '2nd_M' = list(
    country = c('ENG'),
    tier = '2nd',
    gender = 'M',
    non_dom_league_url = NA_character_
  ),
  'non_domestic' = list(
    country = NA_character_,
    tier = NA_character_,
    gender = 'M',
    non_dom_league_url = c(
      'https://fbref.com/en/comps/8/history/Champions-League-Seasons',
      'https://fbref.com/en/comps/19/history/Europa-League-Seasons'
    )
  ),
  'non_domestic' = list(
    gender = 'M',
    non_dom_league_url = c(
      'https://fbref.com/en/comps/8/history/Champions-League-Seasons',
      'https://fbref.com/en/comps/19/history/Europa-League-Seasons'
    )
  ),
  .id = 'group'
)

