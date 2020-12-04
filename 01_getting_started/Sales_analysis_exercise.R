# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikes_tbl <- read_excel(path="G:/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("G:/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("G:/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
bikes_tbl
orderlines_tbl
bikeshops_tbl
glimpse(orderlines_tbl)

# 4.0 Joining Data ----

left_join(orderlines_tbl, bikes_tbl,by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----

bike_orderlines_joined_tbl$category

#Unique
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()


bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ")%>%
  mutate(total.price = price * quantity) %>%
  
  select(-...1, -gender,-ends_with(".id"))%>%

  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything())%>%
  
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

bike_orderlines_wrangled_tbl

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

library(lubridate)

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>% 
  summarize(sales = sum(total_price)) %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_tbl
# Step 2 - Visualize

library(ggplot2)

sales_by_year_tbl %>%
  ggplot(aes(x = year, y = sales)) +
  geom_col(fill = "red") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "Year", # Override defaults for x and y
    y = "Revenue"
  )


# 6.2 Sales by Year and Category 2 ----
library(lubridate)
# Step 1 - Manipulate

sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date,category_1,total_price)%>%
  mutate(year=year(order_date))%>%
  group_by(year,category_1)%>%
  summarise(sales=sum(total_price))%>%
  ungroup()%>%
  
  mutate(sales_text=scales::dollar(sales,big.mark = ".",decimal.mark = ",", prefix="",suffix ="€"))

sales_by_year_cat_1_tbl

# Step 2 - Visualize

sales_by_year_cat_1_tbl %>%
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  geom_col()+
  facet_wrap(~ category_1) +
  geom_smooth(method = "lm", se = FALSE)+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )

# 7.0 Writing Files ----

library("writexl")

# 7.1 Excel ----
bike_orderlines_wrangled_tbl %>%
  write_xlsx("C:/Users/rueta/Desktop/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bike_orderlines_exercise1.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("C:/Users/rueta/Desktop/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bike_orderlines_exercise.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("C:/Users/rueta/Desktop/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bike_orderlines_exercise.rds")



