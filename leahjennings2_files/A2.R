install.packages("tidyverse")
library(tidyverse)

options(scipen = 999)

unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")

data_join <- full_join(unicef_indicator_1, unicef_metadata, by = join_by(country, alpha_2_code, alpha_3_code, numeric_code))
data_join <- full_join(data_join, unicef_indicator_2, by = join_by(country, alpha_2_code, alpha_3_code, numeric_code))

data_join <- unicef_indicator_1 %>%
  full_join(unicef_metadata, by = join_by(country, alpha_2_code, alpha_3_code, numeric_code)) %>%
  full_join(unicef_indicator_2, by = join_by(country, alpha_2_code, alpha_3_code, numeric_code))

# Map World
install.packages("maps")            
map_world <- map_data("world")

# Map 1 Maternal Deaths 2020
data_join_2020 <- data_join %>%
  filter(year ==2020)

map_data_join_2020 <- full_join(data_join_2020, map_world, by = c("country" = "region"))            

ggplot(map_data_join_2020) + 
  aes(x = long, y = lat, group = group, fill = no_deaths) +
  geom_polygon() +
  scale_fill_gradient(low = "pink", high = "firebrick", na.value = "grey90", name = "Number of Maternal Deaths", limits = c(2, 85000), trans = "log10") +
  labs(title = "Overview of Maternal Deaths, 2020") +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Map 2 Life Expectancy 2020
ggplot(map_data_join_2020) + 
  aes(x = long, y = lat, group = group, fill = life_exp) +
  geom_polygon() + 
  scale_fill_gradient(low = "firebrick", high = "pink", na.value = "grey90", name = "Life Expectancy (years)") + 
  labs(title = "Overview of Life Expectancy, 2020") + 
  theme(plot.title = element_text(size = 16, face = "bold"))

# Bar Plot No. of Countries legal frameworks range
unicef_indicator_1 <- unicef_indicator_1 %>%
  mutate(percent_range = cut(percent_legal_frameworks, breaks = c(0, 33, 66, 100), labels = c("<33%", "33-66%", ">66%")))

percent_legal_data <- unicef_indicator_1 %>%
  filter(time_period %in% c(2018, 2020, 2022))

percentage_counts <- percent_legal_data %>%
  group_by(time_period, percent_range) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(percentage_counts, aes(x = factor(time_period), y = count, fill = percent_range)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("<33%" = "firebrick", "33-66%" = "coral", ">66%" = "pink")) +
  labs(title = "Evolution of Legal Frameworks Promoting, Enforcing and Monitoring Gender Equality",
       x = "Year",
       y = "Number of Countries",
       fill = "Percentage Range") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(size = 16, face = "bold"))

#Bar Plot 2 Maternal Death Rnages 
data_join_filtered <- data_join %>%
  filter(!is.na(no_deaths))

data_join_filtered %>%
  mutate(death_range = cut(no_deaths, 
                           breaks = c(-Inf, 100, 5000, Inf), 
                           labels = c("Less than 100", "Between 100 and 5,000", "More than 5,000"),
                           include.lowest = TRUE)) %>%
  group_by(death_range) %>%
  summarise(num_countries = n()) %>%
  filter(!is.na(death_range)) %>%
  ggplot(aes(x = death_range, y = num_countries, fill = death_range)) +
  geom_bar(stat = "identity") +
  labs(title = "Maternal Mortality by Country: Breakdown by Death Ranges",
       x = "Maternal Death Ranges",
       y = "Number of Countries", 
       fill = "Maternal Death Range") +
  scale_fill_manual(values = c("Less than 100" = "pink",
                               "Between 100 and 5,000" = "coral",
                               "More than 5,000" = "firebrick")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Scatter Plot Showing Relationship Between Maternal Deaths and Population 2020
data_join_2020$no_deaths <- as.numeric(as.character(data_join_2020$no_deaths))
options(scipen = 999)
data_join_2020 %>%
  ggplot() + 
  aes(population, no_deaths, color = no_deaths, size = population) + 
  geom_point(alpha = 0.7) +
  scale_color_gradient(name = "Number of Maternal Deaths", low = "lightpink", high = "darkred") +
  labs(title = "Maternal Deaths vs Population in 2020",
       x = "Population",
       y = "Number of Maternal Deaths",
       size = "Population") +
  scale_size_continuous(name = "Population (Size of Dots)") +  # Format population axis labels with commas
  scale_y_continuous(labels = scales::comma) +  # Format number of deaths axis labels with commas
  theme_minimal() + 
  theme(plot.title = element_text(size = 16, face = "bold")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_smooth(method = "lm", se = FALSE, color = "deeppink4")

# Chart showing Top 5 and Bottom 5 Countries (Maternal Deaths)
highest_deaths <- unicef_indicator_2 %>%
  arrange(desc(no_deaths)) %>%
  head(5)

lowest_deaths <- unicef_indicator_2 %>%
  arrange(no_deaths) %>%
  head(5)

highest_countries <- highest_deaths$country
lowest_countries <- lowest_deaths$country

ten_countries <- data_join %>%
  filter(country %in% c(highest_countries, lowest_countries))

ten_life_exp <- unicef_metadata %>%
  filter(country %in% ten_countries$country)

ten_life_exp <- ten_life_exp %>%
  mutate(color_group = ifelse(country %in% highest_countries, "Highest", 
                              ifelse(country %in% lowest_countries, "Lowest", "Other")),
         color = ifelse(color_group == "Highest", "firebrick", ifelse(color_group == "Lowest", "hotpink", "black")))

top_bottom_countries <- rbind(mutate(highest_deaths, category = "Top"),
                              mutate(lowest_deaths, category = "Bottom"))
ggplot(top_bottom_countries, aes(x = reorder(country, -no_deaths), y = no_deaths, color = category)) +
  geom_point(size = 3) +
  geom_text(aes(label = no_deaths), vjust = -0.5, size = 3) +
  labs(title = "Comparison of Maternal Deaths Between Top and Bottom 5 Countries",
       x = "Country",
       y = "Number of Deaths") +
  scale_color_manual(values = c("Top" = "firebrick", "Bottom" = "hotpink"),
                     name = "Category",
                     labels = c("Top 5", "Bottom 5")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 16, face = "bold"))

# Time Series 
ggplot(ten_life_exp, aes(x = year, y = life_exp, color = color_group, group = country)) +
  geom_line() +
  labs(title = "Evolution of Life Expectancy in the Top and Bottom 5 Countries",
       x = "Year",
       y = "Life Expectancy",
       color = "Country Group (By Maternal Deaths") +  # Change legend title
  scale_color_manual(values = c("Highest" = "firebrick", "Lowest" = "hotpink"),
                     breaks = c("Highest", "Lowest")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))  # Corrected placement of plot title

