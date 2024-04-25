library(tidyverse)
library(ggplot2)
library(maps)
library(dplyr)
install.packages("countrycode")
install.packages("plotly")
library(countrycode)
library(plotly)


# Load the data
indicator1 <- read.csv('unicef_indicator_1.csv')
indicator2 <- read.csv('unicef_indicator_2.csv')
metadata <- read.csv('unicef_metadata.csv')

# Combine the datasets
indicator <- bind_rows(indicator1, indicator2)
indicator_main <- full_join(indicator1, metadata, by = "country")
indicator_2018 <- indicator1 %>%
  filter(time_period == 2018)


filtered_data <- indicator_main %>%
  filter(obs_value > 50,  # Example filter condition, adjust according to your needs
         year >= 2018, year <= 2020) %>%  # Filtering by time period
  select(sex, time_period, obs_value, Life_expectancy_at_birth_total_years, country)

# Prepare the world map data
world_map <- map_data("world")
indicator_map <- full_join(world_map, indicator_2018, by = c("region" = "country"), relationship = "many-to-many")
# World Map Chart with more detailed aesthetics
ggplot(data = indicator_map, aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_gradient(low = "#ffb671", high = "#f88501", na.value = "#ffd7b7") +
  
  #scale_fill_viridis_c(option = "C", direction = -1, name = "Reading Proficiency (%)") +
  labs(
    title = "Global Reading Proficiency at Lower Secondary Level",
    subtitle = "percentage of reading proficiency for the year 2018 is shown",
    x = "",
    y = "",
    ) +
  theme_classic() +
  theme(legend.position = "bottom")

# Bar Chart of the Top 10 Countries with improved visuals
top_countries <- indicator1 %>%
  filter(sex == "Total", obs_value > 80 & obs_value < 99) %>% 
  arrange(desc(obs_value)) %>%
  slice_head(n = 10)



custom_colors <- c("#974700", "#ff7f0e", "#f88501", "#f7a156", "#e49a59", "#f0b27d", "#ffa775", "#f8bc99", "#f8bc99", "#f8bc99" ) #defining custom color sets first
ggplot(top_countries, aes(x = reorder(country, obs_value), y = obs_value, fill = country)) +
  geom_col(show.legend = FALSE, width = 0.5) +
 # coord_flip() +
  scale_y_continuous(limits = c(82, 90), breaks = seq(82, 90, by = 1),  oob = scales::oob_squish) +
  
 
  labs(title = "Showing Top 10 Countries by Reading Proficiency", x = "Countries", y = "Proportion (%)",
       subtitle = "These are the nations where more than 80% of students graduate from lower secondary school with at least a basic reading competency. ") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = custom_colors)

# Enhanced Scatterplot with Linear Regression Line and tooltips


#scatter_data <- indicator1 %>%
  #filter(indicator1, by = "country")
indicator_continent <- indicator1 %>% 
  mutate(continent = countrycode(country, "iso3c", "continent"))

scatter_data <- indicator1 %>%
  group_by(country, sex) %>%
  filter(any(obs_value > 50)) %>%
  slice_head(n = 25)

ggplot(scatter_data, aes(x = country, y = obs_value, color = sex)) +
  geom_point(alpha = 50, size = 3.8, color = "orange") +
  #geom_smooth(method = "lm") +
  #geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "solid", size = 2) +
  
  geom_smooth(method = "lm", aes(group = sex), se = FALSE, 
              color = "darkorange", size = 1, linetype = "solid") +
  
  labs(title = "Reading Proficiency",
       subtitle = "showing something...............",
       x = "Countries", y = "Reading Proficiency (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "",
        panel.grid.major = element_blank(),   # Removed major grid lines
    panel.grid.minor = element_blank(),  # Removed minor grid lines
    panel.background = element_blank())




# Time-Series Chart with better legend and labels
time_series_data <- indicator %>%
  filter(indicator == "Proportion of students at the end of lower secondary achieving at least a minimum proficiency level in reading", 
         year == 2018) %>%
  arrange(country, sex)

ggplot(time_series_data, aes(x = sex, y = obs_value, group = country, color = country)) +
  geom_line() +
  scale_color_viridis_d(begin = 0.2, end = 0.8, option = "C") +
  labs(
    title = "Trend of Reading Proficiency Over Years",
    subtitle = "Tracking changes in reading proficiency at the lower secondary level across countries",
    x = "Year",
    y = "Proportion (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold")
  ) +
  facet_wrap(~country, scales = "free_y", ncol = 2) +
  guides(color = guide_legend(override.aes = list(size = 2)))





top_countries_2018 <- indicator %>%
  filter(time_period == 2018, obs_value > 50, obs_value < 99) %>%
  group_by(country) %>%
  summarize(avg_obs_value = mean(obs_value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(avg_obs_value)) %>%
  top_n(10, avg_obs_value) %>%
  select(country) 


time_series_data <- indicator %>%
  filter(time_period == 2018, obs_value > 50, obs_value < 99) %>%
  semi_join(top_countries_2018, by = "country") %>%
  arrange(country, sex)



ggplot(time_series_data, aes(x = sex, y = obs_value, group = sex, color = sex)) +
  geom_line(size = 1) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, option = "C") +
  labs(
    title = "Trend of Reading Proficiency in 2018",
    subtitle = "Top 10 countries by average proficiency, split by gender",
    x = "Gender",
    y = "Proportion (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title = element_text(face = "bold")
  ) +
  facet_wrap(~country, scales = "free_y", ncol = 2) +
  guides(color = guide_legend(override.aes = list(size = 2)))


#**888888888888888888888

country_list <- c("Australia", "Canada", "France", "Germany", "Italy", 
                         "Japan", "New Zealand", "Norway", "United Kingdom", "United States")
country_colors <- c("Australia" = "#E41A1C", "Canada" = "#377EB8", "France" = "#4DAF4A", 
                    "Germany" = "#984EA3", "Italy" = "#FF7F00", "Japan" = "#FFFF33",
                    "New Zealand" = "#A65628", "Norway" = "#F781BF", "United Kingdom" = "#999999",
                    "United States" = "#66C2A5")


filtered_metadata <- metadata %>%
  filter(country %in% country_list, time_period >= 2010 & time_period <= 2020) %>%
  select(country, time_period, Life_expectancy_at_birth_total_years)

filtered_metadata$time_period <- as.integer(filtered_metadata$time_period)



ggplot(filtered_metadata, aes(x = time_period, y = Life_expectancy_at_birth_total_years, group = country, color = country)) +
  geom_line(size = 0.8) +  # Correct usage of geom_line without stat
  geom_point() + # Adding points to the lines for clearer visualization of data points
  scale_color_manual(values = country_colors) +
  scale_x_continuous(breaks = seq(min(filtered_metadata$time_period), max(filtered_metadata$time_period), 1)) +
  labs(
    title = "Life Expectancy Trends from 2010 to 2019",
    subtitle = "Comparison among Selected Developed Countries",
    x = "Year",
    y = "Life Expectancy at Birth (Years)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 12)
  )

#plotly_demo <- ggplotly(timegraph, tooltip = "text")

