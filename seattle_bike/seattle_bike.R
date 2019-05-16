library(tidyverse)
library(lubridate)

# Importing data
bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

# Removing ped_count and reformating data
bike_traffic <- bike_traffic %>% 
  select(-ped_count) %>%
  mutate(date = lubridate::mdy_hms(date),
         year = year(date),
         day = wday(date, label = TRUE),
         day_type = ifelse(day %in% c("Sat", "Sun"), "weekend", "weekday"),
         direction = fct_relevel(direction, c("North", "East", "South", "West")))

#Violin Plot
bike_traffic %>% 
  filter(year %in% 2014:2018) %>% 
  ggplot(aes(x = crossing, y = hour(date), weight = bike_count, colour = direction)) + 
  geom_violin(alpha = 0, scale = "width", position = "identity") +
  facet_grid(cols = vars(day_type)) +
  scale_y_continuous(breaks = c(0,6,12,18,24),
                     limits = c(0,23)) +
  labs(y = "Hour",
       x = "Crossing",
       title = "Seattle bike hourly profile (2014-2018)") +
  coord_flip() 
