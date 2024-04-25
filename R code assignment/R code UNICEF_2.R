install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("maps")
library(tidyverse)
library(ggplot2)
data_left <- read.csv (file = "unicef_indicator_1.csv")
data_right <- read.csv(file = "unicef_indicator_2.csv")
data_main <- read.csv(file = "unicef_metadata.csv")

data_join <- full_join(data_left, data_right)


#world map
latest_data <- data_join %>% 
  group_by(country) %>% 
  filter(time_period == max(time_period))


world_map <- map_data("world")

ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "orange", color = "black", size = 0.15) +
  theme_minimal() +
  ggtitle("World Map")

##bar chart

ggplot(data = latest_data, aes(x = country, y = obs_value)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Bar Chart", x = "Country", y = "obs_value")


combined_data <- data_left %>%
  inner_join(data_main, by = "country")



scatter_plot <- ggplot(combined_data, aes(x = MetadataValue, y = IndicatorValue)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship Between Metadata and Indicator Values",
       x = "Metadata Value",
       y = "Indicator Value",
       caption = "Data source: UNICEF") +
  theme_minimal()