library(RSQLite)
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

sp_500 <- url%>%
  # read the HTML from the webpage
  read_html() %>%
  html_nodes(css = "#constituents") %>%
  html_table() %>% 
  .[[1]]%>% 
  as_tibble()


url <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"

rank <- url%>%
  read_html() %>%
  html_nodes(css = ".titleColumn") %>%
  html_text()%>% 
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  as.numeric()
rank

title<- url%>%
  read_html() %>%
  html_nodes(css = ".titleColumn > a") %>%
  html_text()
title

year<- url%>%
  read_html() %>%
  html_nodes(css = ".titleColumn .secondaryInfo") %>%
  html_text() %>%
  stringr::str_extract("(?<=\\()[0-9]*(?=\\))")%>%
  as.numeric()
year


people <- url %>% 
  read_html() %>%
  html_nodes(".titleColumn > a") %>% 
  html_attr("title")

rating<- url%>%
  read_html() %>%
  html_nodes(css = ".imdbRating > strong") %>%
  html_text() %>%
  as.numeric()
rating

num_ratings<- url %>% 
  read_html() %>%
  html_nodes(css = ".imdbRating > strong") %>% 
  html_attr('title') %>% 
  stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
  stringr::str_replace_all(pattern = ",", replacement = "") %>% 
  as.numeric()
num_ratings

imdb_tbl <- tibble(rank, title, year, people, rating, num_ratings)


























