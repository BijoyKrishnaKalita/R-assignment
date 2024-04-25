install.packages("tidyverse")
install.packages("ggplot2")
install.packages("maps")
library(tidyverse)
library(ggplot2)

data_left <- read.csv (file = "unicef_indicator_1.csv")
data_right <- read.csv(file = "unicef_indicator_2.csv")
data_main <- read.csv(file = "unicef_metadata.csv")

data_join <- full_join(data_left, data_right)


latest_data <- data_join %>% 
  group_by(country) %>% 
  filter(time_period == max(time_period))

indicator <- latest_data %>%
    filter(time_period == 2020)
map_world <- map_data("world")

map_world$country <- tolower(map_world$region)


map_pop_2020 <- full_join(map_world, indicator, by = c("country" = "country")
ggplot(data = map_pop_2020) +
  aes(x = long, y = lat, group = group, fill = obs_value) + # Ensure obs_value exists in map_pop_2020
  geom_polygon() +
  scale_fill_gradient(low = "#44ff44", high = "#ffc916", na.value = "grey") +
  labs(
    title = "Differences in Reading Proficiency in 2020",
    subtitle = "Countries in grey have no data available",
    caption = "Source: UNICEF Indicators",
    x = "Longitude",
    y = "Latitude",
    fill = "Reading Proficiency"
  ) +
  theme_minimal()
