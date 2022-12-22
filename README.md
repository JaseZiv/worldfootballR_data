# worldfootballR_data <img src="man/figures/logo.png" align="right" width="181" height="201"/>

<!-- badges: start -->
![run_scrapes](https://github.com/JaseZiv/worldfootballR_data/workflows/run_extracts/badge.svg)
<!-- badges: end -->

# worldfootballR_data
Repository to hold various data sets scraped from the sites supported in the [`worldfootballR`](https://github.com/JaseZiv/worldfootballR) package. Current sites include:

* fbref.com
* transfermarkt.com
* understat.com
* fotmob.com

***

## Show your support
Follow me on Twitter ([jaseziv](https://twitter.com/jaseziv)) for updates

If this package helps you, all I ask is that you star this repo. If you did want to show your support and contribute to server time and data storage costs, feel free to send a small donation through the link below.

<a href="https://www.buymeacoffee.com/jaseziv83A" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Coffee (Server Time)" height="41" width="174"></a>

***

## The Data

The data can be split up into two main categories:

### 1. Supporting data to help with the functions in `worldfootballR`:

* [Fbref Comps and Leagues](https://github.com/JaseZiv/worldfootballR_data/raw/master/raw-data/all_leages_and_cups/all_competitions.csv)
* [Transfermarkt Leagues](https://github.com/JaseZiv/worldfootballR_data/raw/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv)
* [Mapping between FBref and Transfermarkt Players](https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/fbref-tm-player-mapping/output/fbref_to_tm_mapping.csv)


### 2. Data sets used in the `load_` functions in `worldfootballR`:

* [FBref Big 5 League Advanced season stats](https://github.com/JaseZiv/worldfootballR_data/tree/master/data/fb_big5_advanced_season_stats)

For players and teams, all advanced statistic data available on the site
  
* [FBref match results - Domestic Leagues](https://github.com/JaseZiv/worldfootballR_data/tree/master/data/match_results)

Includes match results played for all domestic leagues available on the site, for all years match results are listed under the fixtures section of leagues

* [FBref match results - International matches and domestic cups](https://github.com/JaseZiv/worldfootballR_data/tree/master/data/match_results_cups)

Includes match results played for all domestic cups and international matches available on the site, for all years match results are listed under the fixtures section of cups/comps

* [Understat shot locations for the Big 5 leagues and RFPL](https://github.com/JaseZiv/worldfootballR_data/tree/master/data/understat_shots)

Shooting data and locations for the big 5 leagues and the RFPL since the 2014/15 seasons.

Shout out to [Mark Wilkins](https://twitter.com/biscuitchaser) for supplying the original data dump of the seasons for all big 5 leagues from 2014/15 to 2021/22. The data was originally [here](https://github.com/Markjwilkins/Understat)
