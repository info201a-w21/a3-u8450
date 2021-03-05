library(stringr)
library(tidyverse)
library(dplyr)
library(lintr)
library(ggplot2)
library(maps)
library(mapproj)
library(kableExtra)

# Load the *county level* data into a variable. 
county_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

############################# Intro + Summary ################################
# I am interested in learning the difference in incarceration rate between
# different races
# In order to do that, I am looking at data of population and jail_pop for 
# white, black, latinx, and other races

# Percentage of incarcerated people in by state, regardless of race
total_perc_incarcerated <- county_data %>%
  filter(total_jail_pop != "NA") %>%
  filter(total_prison_pop != "NA") %>%
  group_by(state) %>%
  summarise(mean_pop = mean(total_pop_15to64),
              total = mean(total_jail_pop + total_prison_pop) / mean_pop) %>%
  select(state, total)

# Percentage of incarcerated black people in jail by state
black_perc_incarcerated <- county_data %>%
  filter(black_jail_pop != 0 | black_jail_pop != "NA") %>%
  filter(black_pop_15to64 != 0 | black_pop_15to64 != "NA") %>%
  filter(black_prison_pop != 0 | black_prison_pop != "NA") %>%
  group_by(state) %>%
  summarise(mean_pop = mean(black_pop_15to64),
            black = mean(black_jail_pop + black_prison_pop) / mean_pop) %>%
  select(state, black)


# Percentage of incarcerated white people in jail by state
white_perc_incarcerated <- county_data %>%
  filter(white_jail_pop != 0 | white_jail_pop != "NA") %>%
  filter(white_pop_15to64 != 0 | white_pop_15to64 != "NA") %>%
  filter(white_prison_pop != 0 | white_prison_pop != "NA") %>%
  group_by(state) %>%
  summarise(mean_pop = mean(white_pop_15to64),
            white = mean(white_jail_pop + white_prison_pop) / mean_pop) %>%
  select(state, white)

# Percentage of incarcerated latinx people in jail by state
latinx_perc_incarcerated <- county_data %>%
  filter(latinx_jail_pop != 0 | latinx_jail_pop != "NA") %>%
  filter(latinx_pop_15to64 != 0 | latinx_pop_15to64 != "NA") %>%
  filter(latinx_prison_pop != 0 | latinx_prison_pop != "NA") %>%
  group_by(state) %>%
  summarise(mean_pop = mean(latinx_pop_15to64),
            latinx = mean(latinx_jail_pop + latinx_prison_pop) / mean_pop) %>%
  select(state, latinx)

# Percentage of incarcerated native people in jail by state
native_perc_incarcerated <- county_data %>%
  filter(native_jail_pop != 0 | native_jail_pop != "NA") %>%
  filter(native_pop_15to64 != 0 | native_pop_15to64 != "NA") %>%
  filter(native_prison_pop != 0 | native_prison_pop != "NA") %>%
  group_by(state) %>%
  summarise(mean_pop = mean(native_pop_15to64),
            native = mean(native_jail_pop + native_prison_pop) / mean_pop) %>%
  select(state, native)

########################## Trend over time chart #############################
# whole lot of trial and error for data cleaning
total_prop <- county_data %>%
  filter(year >= "2000", year <= "2018") %>%
  filter(total_jail_pop != "NA") %>%
  filter(total_prison_pop != "NA") %>%
  group_by(year) %>%
  summarise(mean_pop = mean(total_pop_15to64),
            total = mean(total_jail_pop + total_prison_pop) / mean_pop) %>%
  select(year, total)

white_prop <- county_data %>%
  filter(year >= "2000", year <= "2018") %>%
  filter(white_jail_pop != 0 | white_jail_pop != "NA") %>%
  filter(white_pop_15to64 != 0 | white_pop_15to64 != "NA") %>%
  filter(white_prison_pop != 0 | white_prison_pop != "NA") %>%
  group_by(year) %>%
  summarise(mean_pop = mean(white_pop_15to64),
            white = mean(white_jail_pop + white_prison_pop) / mean_pop) %>%
  select(year, white)

black_prop <- county_data %>%
  filter(year >= "2000", year <= "2018") %>%
  filter(black_jail_pop != 0 | black_jail_pop != "NA") %>%
  filter(black_pop_15to64 != 0 | black_pop_15to64 != "NA") %>%
  filter(black_prison_pop != 0 | black_prison_pop != "NA") %>%
  group_by(year) %>%
  summarise(mean_pop = mean(black_pop_15to64),
            black = mean(black_jail_pop + black_prison_pop) / mean_pop) %>%
  select(year, black)

native_prop <- county_data %>%
  filter(year >= "2000", year <= "2018") %>%
  filter(native_jail_pop != 0 | native_jail_pop != "NA") %>%
  filter(native_pop_15to64 != 0 | native_pop_15to64 != "NA") %>%
  filter(native_prison_pop != 0 | native_prison_pop != "NA") %>%
  group_by(year) %>%
  summarise(mean_pop = mean(native_pop_15to64),
            native = mean(native_jail_pop + native_prison_pop) / mean_pop) %>%
  select(year, native)

latinx_prop <- county_data %>%
  filter(year >= "2000", year <= "2018") %>%
  filter(latinx_jail_pop != 0 | latinx_jail_pop != "NA") %>%
  filter(latinx_pop_15to64 != 0 | latinx_pop_15to64 != "NA") %>%
  filter(latinx_prison_pop != 0 | latinx_prison_pop != "NA") %>%
  group_by(year) %>%
  summarise(mean_pop = mean(latinx_pop_15to64),
            latinx = mean(latinx_jail_pop + latinx_prison_pop) / mean_pop) %>%
  select(year, latinx)

# Trend over time data frame for Total, white and Black
time_trend <- data.frame(total_prop) 
time_trend <- time_trend %>%
  full_join(white_prop, by = "year") %>%
  full_join(black_prop, by = "year") %>%
  full_join(native_prop, by = "year") %>%
  full_join(latinx_prop, by = "year")

# reshape the df
df <- pivot_longer(time_trend, cols = -year, names_to = "metric", values_to = "value")

# ploting
time_trned_plot <- ggplot(data = df) +
  geom_line(mapping = aes(x = year, y = value, group = metric, color = metric))+
  labs(title = "Incarceration rate for different races") +
  labs(x = "2000 - 2018") +
  labs(y = "Incarceration rate(%)")


####################### Variable comparison chart ##########################

# data frame that compares incarceration prop for different races
# in different states
incarceration_df <- data.frame(total_perc_incarcerated)
incarceration_df <- incarceration_df %>%
  full_join(white_perc_incarcerated, by = "state") %>%
  full_join(native_perc_incarcerated, by = "state") %>%
  full_join(latinx_perc_incarcerated, by = "state") %>%
  full_join(black_perc_incarcerated, by = "state") %>%
  arrange(black) %>%
  top_n(10)

# change df to long format
df_state <- pivot_longer(incarceration_df, cols = -state, names_to = "metric", values_to = "value")

state_plot <- ggplot(data = df_state) +
  geom_point(mapping = aes(x = state, y = value, group = metric, color = metric)) +
  labs(title = "Race and incarceration rate in top ten states") +
  labs(x = "States with highest black incarceration rate") +
  labs(y = "Incarceration rate(%)")

####################### Map for states and disparity #########################
# make a data frame 
disparity_df <- data.frame(total_perc_incarcerated)
disparity_df <- disparity_df %>%
  full_join(black_perc_incarcerated, by = "state") %>%
  mutate(ratio = black / total) %>%
  select(state, ratio)

# change state name to abbreviation
state_shape <- map_data("state") %>%
  rename(state = region)

state_shape$state <- state.abb[match(state_shape$state, tolower(state.name))]

# merging
map_df <- disparity_df %>%
  left_join(state_shape, by = "state")

# blank theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Actual mapping

disparity_map <- ggplot(map_df) +
  geom_polygon(
    mapping = aes(x = long, y = lat,  group = group, fill = ratio),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_df$ratio)), na.value = "white",
                        low= "sky blue", high = "purple") +
  blank_theme +
  ggtitle("Ratio of black incarceration rate compare to overall")

############################ Summary data for Markdown ######################
incarceration_table <- data.frame(total_perc_incarcerated)
incarceration_table <- incarceration_table %>%
  filter(state == "WA") %>%
  left_join(white_perc_incarcerated) %>%
  left_join(native_perc_incarcerated) %>%
  left_join(latinx_perc_incarcerated) %>%
  left_join(black_perc_incarcerated)
  