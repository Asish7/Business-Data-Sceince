library(tidyverse)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
world <- map_data("world")

covid_data_tbl%>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))

covid_data_tbl <- covid_data_tbl %>%
  rename(cumulative_cases = `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`, continent=continentExp, region=countriesAndTerritories)


combined_data_tbl <- merge(x = covid_data_tbl, y = world, 
                                by ="region",
                                all.x = TRUE, 
                                all.y = FALSE) 


covid_data_tbl_reshaped <- combined_data_tbl %>%
  
  select(month, geoId, continent, deaths, popData2019,long,lat) %>%
  
  # Group by category and summarize
  group_by(geoId, month, popData2019) %>%
  summarise(deaths_total =sum(deaths)) %>%
  mutate(mortality_rate = (deaths_total / popData2019)*100)%>%
  ungroup()

world <- dplyr::left_join(world, covid_data_tbl_reshaped , by=c("region" = "country_new"))


world %>%
  ggplot(aes(map_id = region)) +
  geom_map(aes(fill = Mortality_Rate), map = world, color = "white") +
  expand_limits(x = world$long, y = world$lat) +
  scale_fill_gradient(high = "#541e2b", low = "#ff8282", na.value = "grey50",  guide = "colorbar")
 

