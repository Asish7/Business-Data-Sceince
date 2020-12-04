library(data.table)
library(tidyverse)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)


#test_df <- data.frame(matrix(runif(10000000), nrow=1000000))
#write.csv(test_df, 'test_df.csv', row.names = F)

#system.time({test_df_base <- read.csv("test_df.csv")})
#system.time({test_df_readr <- read_csv("test_df.csv")})
#system.time({test_dt <- fread("test_df.csv")})

test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)

covid_data_dt[year == 2019, sum(cases), by = continentExp]

test <- covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]
covid_data_dt[1:2]


test <-covid_data_dt[order(year, month, day, -countriesAndTerritories)]

test <- covid_data_dt[,geoId]

test <- covid_data_dt[,c("geoId", "countriesAndTerritories")]

test <- covid_data_dt[,list(geoId)]
# Short form using .
covid_data_dt[,.(geoId)]
# Select multiple columns
covid_data_dt[,.(geoId, countriesAndTerritories)]

# Rename them directly
covid_data_dt[,.(CountryCode = geoId, country = countriesAndTerritories)]

# select columns named in a variable using the ..prefix
select_cols = c("cases", "deaths")
covid_data_dt[, ..select_cols]

colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")
colnames(covid_data_dt)
