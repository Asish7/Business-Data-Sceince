library(RSQLite)
library(tidyverse)
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "C:/Users/rueta/Desktop/Data Science Project-R/DS_101/00_data/02_chinook/Chinook_Sqlite.sqlite")

dbListTables(con)
tbl(con, "Album")
album_tbl <- tbl(con, "Album") %>% collect()

# 
# res <- dbSendQuery(con, "SELECT * FROM Album")
# dbFetch(res)
# dbClearResult(res)

dbDisconnect(con)
con
album_tbl

library(glue)
library(httr)
library(jsonlite)

# resp <- GET("https://swapi.dev/api/people/1/")

sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}
resp <- sw_api("/people/1")
resp

web_content <- rawToChar(resp$content)
web_content
# json_data <- fromJSON("https://swapi.dev/api/planets/1/")
# my_data <- as.data.frame(json_data)
# my_data

data <- resp %>% 
  .$content %>% 
  rawToChar()%>% 
  fromJSON()

resp <- GET('https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE')
resp


token    <- "my_individual_token"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))
response

alphavantage_api_url <- "https://www.alphavantage.co/query"
ticker               <- "WDI.DE"

library(keyring)
keyring::key_set("token")
GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = key_get("token"))
)



#purrr

library(purrr)

numbers <- c(1:5)
numbers_list <- map(numbers, print)

bike_data_lst <- fromJSON("C:/Users/rueta/Desktop/Data Science Project-R/DS_101/00_scripts/bike_data.json")

#View(bike_data_lst)
#productDetail --> variationAttributes --> values --> [[1]] --> displayValue

bike_data_lst[["productDetail"]][["variationAttributes"]][["values"]][[1]][["displayValue"]]

bike_data_lst %>%
  purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue")





























