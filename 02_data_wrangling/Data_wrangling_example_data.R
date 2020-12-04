aq_dt <- data.table(airquality)
aq_dt[is.na(Ozone), .(Solar.R, Wind, Temp)]

setDT(airquality)
airquality[!is.na(Ozone), .(Solar.R, Wind, Temp)]


covid_data_dt[,sum(deaths > 1000)]

covid_data_dt[, deaths_per_capita := deaths / popData2019]

covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                      cases_per_capita = cases / popData2019,
                      deaths_per_cases = deaths / cases)]

# To delete a column, assign it to NULL
covid_data_dt[, deaths_per_cases := NULL]


data("mtcars") # step not absolutely necessary
mtcars$carname <- rownames(mtcars)

mtcars_dt <- as.data.table(mtcars)
test <- mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

covid_data_dt[country == "Germany" & month == 4, .(m_cases = mean(cases),  m_death = mean(deaths))]

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              length(day)
]

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              .N
]


covid_data_dt[deaths > 1000, .N, by = country]
covid_data_dt[,.I[deaths > 1000]]

test <- covid_data_dt[continent == "Europe",
              .(mean(cases), mean(deaths)),
              by = .(country, month, year)
]

library(magrittr)

mtcars_dt[,.(.N,milage=mean(mpg)),by=gear]

covid_cases_means <- covid_data_dt[,.(m_cases  = mean(cases) %>% round(1), 
                                      m_deaths = mean(deaths) %>% round(1)), 
                                   by = .(country)
]


covid_data_dt[, .(
  m_cases  = round(mean(cases),  digits = 1), 
  m_deaths = round(mean(deaths), digits = 1)
), 
by = .(country)][order(-m_cases)]

covid_data_dt[, .N, 
              .(
                death_gt_1k = deaths > 1000, 
                cases_lt_1k = cases < 1000
              )
]

test <- covid_data_dt[, print(.SD), by = year]



covid_data_dt[, lapply(.SD, mean), by = year]


covid_data_dt[, lapply(.SD, mean), 
              by = .(year, month), 
              .SDcols = c("cases", "deaths")
]

setkey(covid_data_dt, date, country)


# Create a new data.table
covid_data_EUR_dt <- covid_data_dt[ continent == "Europe", 
                                    lapply(.SD, function(x) {
                                      x %>% 
                                        mean() %>% 
                                        round(1)
                                    }
                                    ), 
                                    by = .(country), 
                                    .SDcols = c("cases", "deaths")
]

# Set key
setkey(covid_data_EUR_dt, country)
key(covid_data_EUR_dt)


# Create two data.tables from that
cd_dt1 <- covid_data_EUR_dt[, .(country, cases)]
cd_dt2 <- covid_data_EUR_dt[1:20, .(country, deaths)]

# Join them
cd_dt1[cd_dt2]


# Remove keys
setkey(cd_dt1, NULL)
setkey(cd_dt2, NULL)


# Join
cd_dt1[cd_dt2, on = "country"]


# If they had different colnames
cd_dt1[cd_dt2, on = c(colA = "colB")]


# Alternatively you can use the function merge()
# Inner Join
merge(cd_dt1, cd_dt2, by='country')
# Left Join
merge(cd_dt1, cd_dt2, by='country', all.x = T)
# Outer Join
merge(cd_dt1, cd_dt2, by='country', all = T)


cd_dt1[cd_dt2, on = "country", deaths := i.deaths]


dt_list    <- list(cd_dt1, cd_dt2, cd_dt3)
merge_func <- function(...) merge(..., all = TRUE, by='country')
dt_merged  <- Reduce(merge_func, dt_list)















