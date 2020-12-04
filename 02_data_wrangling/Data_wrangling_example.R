# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bike_orderlines_tbl <- read_excel("C:/Users/rueta/Desktop/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bike_orderlines_exercise.xlsx")
bikeshops_tbl  <- read_excel("C:/Users/rueta/Desktop/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
bikes_tbl <- read_excel(path="C:/Users/rueta/Desktop/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")%>%
  
  # Separate product category name in main and sub
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " . ") %>%
  
  # Renaming columns
  set_names(names(.) %>% str_replace_all("\\.", "_"))

bikes_tbl %>%
  select(bike_id, model, model_year)

bikes_tbl %>%
  select(1:3)

bikes_tbl %>%
  select(1, contains("model"))

bikes_tbl %>%
  select(model, price)


bikes_tbl %>%
  select(category_1:category_3, everything())

bikes_tbl %>%
  relocate(category_1:category_3)


?starts_with

bikes_tbl %>%
  select(starts_with("model"))

bikes_tbl %>%
  # select(price) %>% Does not work
  pull(price) %>%
  mean()

?where

bikes_tbl %>%
  select(where(is.character))

bikes_tbl %>%
  select(where(is.numeric))

bikes_tbl %>%
  select(!where(is.numeric))

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  rename(
    Model           = model,
    `Bike Family`   = category_1,
    `Ride Style`    = category_2,
    `Bike Category` = category_3,
    `Price in Euro` = price
  )

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(c("Model", "Bike Family", "Ride Style", "Bike Category", "Price in Euro"))

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())

bikes_tbl %>%
  select(model, price) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price > mean(price))

bikes_tbl %>%
  select(model, price) %>%
  filter((price > 5000) | (price < 1000)) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price > 5000,
         model %>% str_detect("Endurace")
  )


bikes_tbl %>%
  filter(category_1 %in% c("Hybrid / City", "E-Bikes"))


bikes_tbl %>%
  filter(category_2 == "E-Mountain")

bikes_tbl %>%
  filter(category_2 != "E-Mountain")

bikes_tbl %>%
  filter(!(category_2 %in% c("Hybrid / City", "E-Bikes")))


bikes_tbl1 <- bikes_tbl %>%
  arrange(desc(price)) %>%
  slice(1:5)


bikes_tbl %>%
  arrange(price) %>%
  slice(1:5)


bikes_tbl %>%
  arrange(desc(price)) %>%
  slice((nrow(.)-4):nrow(.))


bikes_tbl %>%
  distinct(category_1)

bikes_tbl %>%
  distinct(category_1, category_2)

bikes_tbl %>%
  distinct(category_1, category_2, category_3)



bike_orderlines_tbl %>%
  mutate(total_price = log(total_price))


bike_orderlines_tbl %>%
  mutate(is_strive = model %>% str_to_lower() %>% str_detect("strive")) %>%
  filter(is_strive)


test <- bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>% 
  select(total_price, price_binned, everything())


bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>%
  mutate(price_binned2 = case_when(
    total_price > quantile(total_price, 0.75) ~ "High",
    total_price > quantile(total_price, 0.25) ~ "Medium",
    TRUE ~ "Low" # Everything else
  )) %>% 
  select(total_price, price_binned, price_binned2, everything())


bike_orderlines_tbl %>%
  mutate(bike_type = case_when(
    model %>% str_to_lower() %>% str_detect("aeroad") ~ "Aeroad",
    model %>% str_to_lower() %>% str_detect("ultimate") ~ "Ultimate",
    TRUE ~ "Not Aeroad or Ultimate" # Everything else
  )) %>% 
  select(bike_type, everything())


bike_orderlines_tbl %>%
  summarise(
    revenue = sum(total_price)
  )

test <- bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(revenue = sum(total_price)) %>%
  # Always ungroup() after you summarise(). Left-over groups will cause difficult-to-detect errors.
  ungroup() %>%
  arrange(desc(revenue))


bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(
    count = n(),
    avg   = mean(total_price),
    med   = median(total_price),
    sd    = sd(total_price),
    min   = min(total_price),
    max   = max(total_price)
  )%>%
  ungroup()%>%
  arrange(desc(count))


bike_data_sizes_tbl %>% 
  select(name, year, price_euro, color, size, stock_availability) %>% 
  pivot_wider(names_from  = size, 
              values_from = stock_availability)


bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
  select(bikeshop, category_1, total_price) %>%
  
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(sales))


bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>%
  pivot_wider(names_from  = category_1,
              values_from = sales) %>%
  mutate(
    Mountain = scales::dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Road     = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `Hybrid / City` = scales::dollar(`Hybrid / City`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `E-Bikes` = scales::dollar(`E-Bikes`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")
  )

bikeshop_revenue_formatted_tbl1 <- bikeshop_revenue_formatted_tbl %>%
  pivot_longer(cols           = c(names(.)[2:6]),
               names_to       = "category_1",
               values_to      = "sales",
               values_drop_na = T) %>%
  mutate(sales =  sales %>% str_remove_all("€|\\.") %>% as.double())


bike_orderlines_tbl %>%
  select(-contains("category")) %>%
  
  bind_cols(
    bike_orderlines_tbl %>% select(category_1)
  )

train_tbl <- bike_orderlines_tbl %>%
  slice(1:(nrow(.)/2))


test_tbl <- bike_orderlines_tbl %>%
  slice((nrow(.)/2 + 1):nrow(.))



train_tbl %>%
  bind_rows(test_tbl)


bike_orderlines_tbl %>% 
  select(order_date) %>% 
  mutate(order_date = as.character(order_date)) %>%
  
  # separate
  separate(col  = order_date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  ) %>%
  unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
  mutate(order_date_united = as.Date(order_date_united))
