# day 05 - slope ---------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(jsonlite)
library(lubridate)


# data import -------------------------------------------------------------

# Function to clean the JSON files that my spotify data is stored in
read_my_streaming_data <- function(file) {
  
  read_json(file, simplifyVector = TRUE) %>% 
    as_tibble() %>% 
    clean_names() %>% 
    mutate(
      end_time = ymd_hm(end_time),
      date = date(end_time)
    )
}

# List of the streaming data files
my_streaming_files <- list.files(here("day05 - slope", "raw data"), full.names = TRUE)

# Map cleaning function to files and bind rows to a tibble
my_streaming_data <- map_dfr(my_streaming_files, read_my_streaming_data)


# data transformation -----------------------------------------------------

# Podcasts to filter out of data
podcasts <- c("Dungeons and Daddies", "You're Wrong About", "Revolutions", "The Daily Zeitgeist", "The Blasting Company")

# Data of my top artists of 2020
top_artists <-
  my_streaming_data %>% 
  filter(!artist_name %in% podcasts) %>% 
  count(artist_name, sort = T) %>% 
  top_n(5)

# Data from the beginning of 2020 / end of 2020
data <- 
  my_streaming_data %>% 
  filter(date <= min(date) + 60 | date >= max(date) - 60) %>% 
  filter(artist_name %in% top_artists$artist_name) %>% 
  mutate(
    time_period = ifelse(date <= min(date) + 30, "Beginning of 2020", "End of 2020")
    ) %>% 
  group_by(time_period) %>% 
  count(artist_name, sort = TRUE) %>% 
  mutate(time_period_num = ifelse(time_period == "Beginning of 2020", 0, 2))

# Write data to csv to link to in alt text on Twitter
write_csv(data, here("day05 - slope", "data", "my_top_artists.csv"))


# build plot --------------------------------------------------------------

# Load fonts
extrafont::loadfonts(device = "win")

# Colour scale palette
morbid_stuff_pal <- c(
  "#ECF2CD",
  "#DAE57B",
  "#779169",
  "#8DC4C1",
  "#B2DDBF"
)

data %>% 
  mutate(
    align = ifelse(time_period_num == 0, "right", "left"),
    axis_nudge = ifelse(time_period_num == 0, -0.25, 2.25)
  ) %>% 
  ggplot(aes(time_period_num, n)) +
  geom_segment(
    aes(x = time_period_num, xend = time_period_num, y = 0, yend = 130),
    color = "#3C4153"
    ) +
  geom_line(
    aes(color = artist_name, group = artist_name),
    size = 2
    ) +
  geom_point(
    aes(color = artist_name),
    size = 4
    ) +
  geom_text(
    aes(x = axis_nudge, label = artist_name, hjust = align),
    size = 3.5, 
    family = "Montserrat SemiBold"
    ) +
  geom_text(
    data = data.frame(),
    aes(x = -2.25, y = 70, label = "MY\nSPOTIFY\nTOP\nARTISTS\nOF 2020"),
    hjust = "right",
    size = 20, 
    color = "#352862",
    family = "Compacta BT"
  ) +
  geom_text(
    data = data %>% distinct(time_period_num, time_period),
    aes(x = time_period_num, y = 135, label = time_period),
    size = 4, 
    color = "#352862",
    family = "Montserrat ExtraBold"
  ) +
  labs(
    caption = "Viz - @MaiaPelletier"
  ) +
  xlim(c(-4.5, 3.5)) +
  ylim(c(-3, 140)) +
  scale_color_manual(values = morbid_stuff_pal) +
  theme_void(base_family = "Montserrat SemiBold") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#F7CDC1", color = NA),
    plot.caption = element_text(hjust = 0.5, size = 6, color = "#352862"),
    plot.margin = margin(10, 15, 10, 15)
  ) + 
  ggsave(here("day05 - slope", "day05_slope.png"), type = "cairo", dpi = 500, height = 6.5, width = 8)

