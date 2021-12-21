library(magrittr)
library(here)

url <- "https://www.transfermarkt.com/real-madrid/mitarbeiterhistorie/verein/418"
history_pg <- xml2::read_html(url)

staff <- history_pg %>% rvest::html_nodes(".auflistung tbody tr td") %>% rvest::html_nodes(".inline-select")

staff_type_text <- staff %>% rvest::html_nodes("select option") %>% rvest::html_text() %>% stringr::str_squish()
staff_type_idx <- staff %>% rvest::html_nodes("select option") %>% rvest::html_attr("value")
staff_types <- data.frame(staff_type_idx = staff_type_idx, staff_type_text = staff_type_text) %>% dplyr::filter(staff_type_text != "")
write.csv(staff_types, here("raw-data", "transfermarkt_staff", "tm_staff_types.csv"), row.names = F)
