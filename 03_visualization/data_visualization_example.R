library(tidyverse) # loads ggplot2
library(lubridate)
library(readxl)

bike_orderlines_tbl <- read_excel("G:/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bike_orderlines_exercise.xlsx")
bike_orderlines_tbl

# Step 1: Format data ----

sales_by_year_tbl <- bike_orderlines_tbl %>%
  
  # Selecting columns to focus on and adding a year column
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  # Grouping by year, and summarizing sales
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%

  # € Format Text
  mutate(sales_text = scales::dollar(sales, 
                                     big.mark     = ".", 
                                     decimal.mark = ",", 
                                     prefix       = "", 
                                     suffix       = " €"))


# Step 2: Plot ----
sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales))

# Without piping 
ggplot(data = sales_by_year_tbl, 
       aes(x     = year, 
           y     = sales, 
           color = sales))



sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE)+
  
  # not what you want because 2 is not a variable
  geom_point(aes(size = 2),
             
             # this is fine -- turns all points red
             color = "red")




# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>%
  
  select(order_id, order_line, total_price, quantity) %>%
  
  group_by(order_id) %>%
  summarize(
    total_quantity = sum(quantity),
    total_price    = sum(total_price)
  ) %>%
  ungroup()

# Scatter Plot
order_value_tbl %>%
  
  ggplot(aes(x = total_quantity, y = total_price)) +
  
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)


# Data Manipulation
revenue_by_month_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year_month = floor_date(order_date, "months")  %>% ymd()) %>%
  
  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Line Plot
revenue_by_month_tbl %>%
  
  ggplot(aes(x=year_month, y=revenue)) +
  
  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = "loess", span = 0.2)



# Data Manipulation
revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, total_price) %>% 
  
  group_by(category_2) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Bar Plot
revenue_by_category_2_tbl %>%
  
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
  
  ggplot(aes(category_2, revenue)) +
  
  geom_col(fill = "#2c3e50") + 
  coord_flip()


# Histogram

bike_orderlines_tbl %>%
  
  distinct(model, price) %>%
  
  ggplot(aes(price)) +
  
  geom_histogram(bins = 25, fill = "blue", color = "white")



# Histogram
bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  
  geom_histogram() +
  
  facet_wrap(~ frame_material, ncol = 1)



# Density
bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  
  geom_density(alpha = 0.5) +
  # facet_wrap(~ frame_material, ncol = 1) +
  
  theme(legend.position = "bottom")



# Data Manipulation
unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, model, price) %>%
  distinct() %>%
  
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

# Box Plot
unit_price_by_cat_2_tbl %>%
  
  ggplot(aes(category_2, price)) +
  
  geom_boxplot() +
  coord_flip()


# Violin Plot & Jitter Plot

unit_price_by_cat_2_tbl %>%
  
  ggplot(aes(category_2, price)) +
  
  geom_jitter(width = 0.15, color = "#2c3e50") +
  geom_violin(alpha = 0.5) +
  
  coord_flip()













# Data Manipulation

revenue_by_year_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year = year(order_date)) %>%
  
  group_by(year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()





# Adding text to bar chart
# Filtering labels to highlight a point

revenue_by_year_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = "#2c3e50") +
  geom_smooth(method = "lm", se = FALSE) +
  
  geom_text(aes(label =  scales::dollar(revenue, 
                                        scale  = 1e-6, 
                                        prefix = "",
                                        suffix = "M")), 
            vjust = 1.5, color = "white") +
  
  geom_label(label =  "Major Demand This Year",
             vjust = -0.5, 
             size  = 5,
             fill  = "#1b71c4",
             color = "white",
             fontface = "italic",
             data = revenue_by_year_tbl %>%
               filter(year %in% c(2019))) + 
  
  expand_limits(y = 2e7)















































































