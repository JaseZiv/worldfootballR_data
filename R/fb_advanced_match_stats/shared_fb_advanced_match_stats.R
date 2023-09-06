library(dplyr)
params <- bind_rows(
  'big5' = list(
    country = c('ENG', 'ESP', 'FRA', 'GER', 'ITA'),
    tier = '1st',
    gender = 'M'
  ),
  'other_1st_M' = list(
    country = 'USA',
    tier = '1st',
    gender = 'M'
  ),
  # 'other_1st_M' = list(
  #   country = c('POR', 'NED', 'BRA', 'MEX', 'USA'),
  #   tier = '1st',
  #   gender = 'M'
  # ),
  # '1st_F' = list(
  #   country = c('ENG', 'USA'),
  #   tier = '1st',
  #   gender = 'F'
  # ),
  # '2nd_M' = list(
  #   country = c('ENG'),
  #   tier = '2nd',
  #   gender = 'M'
  # ),
  .id = 'group'
)
