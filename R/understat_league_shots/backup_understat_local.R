library(worldfootballR)
library(dplyr)
library(janitor)

setwd(paste0(here::here(), "/data/understat_shots"))


leagues <- c("EPL", "La liga", "Bundesliga", "Serie A", "Ligue 1", "RFPL")

for(each_league in leagues) {
  if(each_league == "La liga") {
    each_league_clean <- "La_liga"
  } else if (each_league == "Serie A") {
    each_league_clean <- "Serie_A"
  } else if (each_league == "Ligue 1") {
    each_league_clean <- "Ligue_1"
  } else {
    each_league_clean <- each_league
  }
  
  
  league_name_clean <- janitor::make_clean_names(each_league)
  
  f <- read_worldfootballr_rds(name=paste0(league_name_clean, "_shot_data"), tag = "understat_shots") %>% 
    mutate(minute = as.numeric(minute))
  
  saveRDS(f, paste0(league_name_clean, "_shot_data.rds"))
}







