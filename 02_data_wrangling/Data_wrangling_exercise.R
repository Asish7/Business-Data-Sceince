# Tidyverse
library(tidyverse)
library(vroom)
library(magrittr)
# Data Table
library(data.table)


# 2.0 DATA IMPORT ----

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)


patent_tbl <- vroom(
  file       = "C:/Users/rueta/Desktop/Data Science Project-R/DS_101/02_data_wrangling/patent.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)



col_types1 <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)


assignee_tbl <- vroom(
  file       = "C:/Users/rueta/Desktop/Data Science Project-R/DS_101/02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_names  = names(col_types1),
  col_types  = col_types1,
  na         = c("", "NA", "NULL")
)

assignee_tbl <- assignee_tbl %>%
  rename(assignee_id = id) 


col_types2 <- list(
  id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)


patent_assignee_tbl <- vroom(
  file       = "C:/Users/rueta/Desktop/Data Science Project-R/DS_101/02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_names  = names(col_types2),
  col_types  = col_types2,
  na         = c("", "NA", "NULL")
)

patent_assignee_tbl <- patent_assignee_tbl %>%
  rename(patent_id = id) 

col_types3 <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_character()
)


upsc_tbl <- vroom(
  file       = "C:/Users/rueta/Desktop/Data Science Project-R/DS_101/02_data_wrangling/uspc.tsv", 
  delim      = "\t", 
  col_names  = names(col_types3),
  col_types  = col_types3,
  na         = c(" ", "NA", "NULL")
)

upsc_tbl <- upsc_tbl %>%
  rename(assignee_id = uuid) 


#Patent Dominance

combined_data_assignee <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                       by ="assignee_id",
                       all.x = TRUE, 
                       all.y = FALSE) 


#What US company has the most patents? 
combined_data_assignee %>%
  group_by(organization) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count))%>% head(1)%>%na.omit()


#List the 10 US companies with the most assigned/granted patents.
combined_data_assignee %>%
  group_by(organization) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count))%>% head(11)%>%na.omit()


#Recent patent activity

combined_data_patent <- merge(x = combined_data_assignee, y = patent_tbl, 
                                by.x = "patent_id",  by.y ="number",
                                all.x = TRUE, 
                                all.y = FALSE) 


combined_data_patent_refined <- combined_data_patent %>%
  select(patent_id, organization,date)%>%
  separate(col  = date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  )


combined_data_patent_refined%>%
  group_by(organization) %>%
  filter(  ) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count))%>% head(11)%>%na.omit()





















