
# 3. 
library(tidyverse)
library(rvest)
library(rvest)
library(dplyr)

swim <- read_html("https://en.wikipedia.org/wiki/List_of_world_records_in_swimming")
swim_tables <- swim %>% html_table(., fill = T)

attach(swim_tables)
swim_df <- bind_rows(swim_tables)
swim_df_subset <- swim_df %>%
  slice(1:97) %>%   # select first 97 rows
  select(1:2, 4)    # select columns 1, 2, and 4
write.csv(swim_df_subset, "PS5_Fraser.csv", row.names = FALSE)
#saved at wd("~/Desktop/ECON 5253/DScourseS23/ProblemSets/PS5")

# 4. 
library(tidyverse)
library(fredr)
library(purrr)
library(dplyr)
library(ggplot2)
usethis::edit_r_environ()

fredr_set_key(sys.getenv("FRED_API_KEY"))

us.unrate <-fredr(
  series_id="UNRATE",
  observation_start = as.Date("2019-01-01"),
  observation_end = as.Date("2023-01-01"))
view(us.unrate)

my_df <- us.unrate %>%
  select(1,3)
my_df 

us.civpart <-fredr(
  series_id="CIVPART",
  observation_start = as.Date("2019-01-01"),
  observation_end = as.Date("2023-01-01"))
view(us.civpart)

map_dfr(c("CIVPART", "UNRATE"), fredr) %>%
  ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = "Rate", color = "Series")

