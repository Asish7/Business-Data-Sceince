library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing


#Getting the Urls

url <- "https://www.canyon.com/en-de"
#xopen(url)

url_home <- read_html(url)


url_home

bike_family_tbl<- url_home %>%
  html_nodes(css = ".js-navigationDrawer__list--secondary")%>%
  html_attr('id')%>%
  discard(.p = ~stringr::str_detect(.x,"WMN|WOMEN|GEAR|OUTLET")) %>%
  enframe(name = "position", value = "family_class")%>%
  
  mutate(
    family_id = str_glue("#{family_class}")
  )

bike_family_tbl

# 1.2 COLLECT PRODUCT CATEGORIES ----

family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")
family_id_css

bike_category_tbl <- url_home %>%
  html_nodes(css = family_id_css) %>%
  html_nodes(css = ".navigationListSecondary__listItem .js-ridestyles") %>%
  html_attr('href') %>%
  enframe(name = "position", value = "subdirectory") %>%
  mutate(
    url = glue("https://www.canyon.com{subdirectory}")
  ) %>%
  distinct(url)

bike_category_tbl


bike_category_url <- bike_category_tbl$url[1]

#xopen(bike_category_url)

html_bike_category  <- read_html(bike_category_url)

bike_url_tbl        <- html_bike_category %>%

  html_nodes(css = ".productTile__contentWrapper > a") %>%
  html_attr("href") %>%
  str_remove(pattern = "\\?.*") %>%
  enframe(name = "position", value = "url")
  
bike_url_tbl 


# 2.1.2 Extract the descriptions (since we have retrieved the data already)


bike_desc_tbl <- html_bike_category %>%
  html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
  html_attr("content") %>%
  enframe(name = "position", value = "description")
  
bike_desc_tbl



















