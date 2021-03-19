library(tidyverse)
library(rvest)

fb_country_leagues <- function() {
  
  main_url <- "https://fbref.com"
  countries_page <- xml2::read_html("https://fbref.com/en/countries/")
  
  country_holder <- countries_page %>% rvest::html_nodes("#countries") %>% rvest::html_nodes("tbody") %>% rvest::html_nodes("tr")
  
  idx <- 0
  countries_df <- data.frame()
  
  for(each_row in country_holder) {
    idx <- idx + 1
    countries_df[idx, "country"] <- tryCatch(each_row %>% rvest::html_nodes(".left:nth-child(1) a") %>% rvest::html_text(), error = function(e) NA_character_)
    countries_df[idx, "country_url"] <- tryCatch(each_row %>% rvest::html_nodes(".left:nth-child(1) a") %>% rvest::html_attr("href") %>% paste0(main_url, .) %>% paste(collapse = ",\n"), error = function(e) NA_character_)
    
    if(is_empty(each_row %>% rvest::html_nodes(".right~ .right+ .left") %>% rvest::html_nodes("a") %>% rvest::html_text())) {
      countries_df[idx, "league_name"] <- NA_character_
    } else {
      countries_df[idx, "league_name"] <- tryCatch(each_row %>% rvest::html_nodes(".right~ .right+ .left") %>% rvest::html_nodes("a") %>% rvest::html_text() %>% paste(collapse = ",\n"), error = function(e) NA_character_)
    }
    
    if(is_empty(each_row %>% rvest::html_nodes(".right~ .right+ .left") %>% rvest::html_nodes("a") %>% rvest::html_attr("href"))) {
      countries_df[idx, "league_url"] <- NA_character_
    } else {
      countries_df[idx, "league_url"] <- tryCatch(each_row %>% rvest::html_nodes(".right~ .right+ .left") %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% paste0(main_url, .) %>% paste(collapse = ",\n"), error = function(e) NA_character_)
    }
    
  }
  
  countries_df <- countries_df %>% dplyr::mutate(has_leage_page = !is.na(league_name))
  
  return(countries_df)
}

# scrape the data
countries_df <- fb_country_leagues()

# write the final data
write.csv(countries_df, here::here("raw-data", "countries_list", "countries_df.csv"), row.names = F)
