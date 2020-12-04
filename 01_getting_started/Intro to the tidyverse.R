class(cars)
cars_tbl <- as_tibble(cars)
class(cars_tbl)

library("tidyverse")

iris %>% head(n=3)

tibble(
  x = 1:50,
  y = runif(50), 
  z = x + y^2,
  outcome = rnorm(50)
)

class(cars)
## "data.frame"

cars_tbl <- as_tibble(cars)
class(cars_tbl)
## "tbl_df"     "tbl"        "data.frame"

# This way applies to dataframes and tibbles
vehicles <- as_tibble(cars[1:5,])
vehicles[['speed']]
vehicles[[1]]
vehicles$speed

# Using placeholders with the pipe
vehicles %>% .$dist
vehicles %>% .[['dist']]
vehicles %>% .[[2]]


library(readr)
dataset_tbl <- read_csv("data.csv")

library(tidyverse)
diamonds2 <- readRDS("diamonds2.rds")

diamonds2 %>% head(n = 5)
## # A tibble: 5 x 3
##   cut     `2008` `2009`
##   <chr>    <dbl>  <dbl>
## 1 Ideal      326    332
## 2 Premium    326    332
## 3 Good       237    333
## 4 Premium    334    340
## 5 Good       335    341

diamonds2 %>% 
  pivot_longer(cols      = c("2008", "2009"), 
               names_to  = 'year', 
               values_to = 'price') %>% 
  head(n = 5)

diamonds3 <- readRDS("diamonds3.rds")

diamonds3 %>% head(n = 5)

diamonds3 %>% 
  pivot_wider(names_from  = "dimension",
              values_from = "measurement") %>% 
  head(n = 5)

diamonds4 <- readRDS("diamonds4.rds")

diamonds4

diamonds4 %>% 
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)

diamonds5 <- readRDS("diamonds5.rds")

diamonds5

diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')


library(ggplot2) # To load the diamonds dataset
library(dplyr)
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)

diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:5)

diamonds %>% 
  arrange(cut, carat, desc(price))

diamonds %>% 
  select(color, clarity, x:z) %>% 
  head(n = 5)

diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)

diamonds %>% 
  select(x:z, everything()) %>% 
  head(n = 5)

diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head(n = 5)

diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)

diamonds %>% 
  group_by(cut) %>% 
  summarize(max_price  = max(price),
            mean_price = mean(price),
            min_price  = min(price))

library(lubridate)
ymd(20101215)
mdy("4/1/17")

bday <- dmy("14/10/1979")
month(bday)
