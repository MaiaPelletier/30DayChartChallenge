# day 07 - physical ---------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(showtext)

showtext_auto()
font_add_google("Open Sans Condensed", family = "opensanscond", regular.wt = 300)

# data import -------------------------------------------------------------

col_types <- spec_csv("day07 - physical/raw data/en_climate_daily_ON_6106001_2018_P1D.csv")

ott_weather <- 
  map_dfr(
    list.files("day07 - physical/raw data", full.names = T), 
    read_csv, 
    col_types = col_types
  ) %>% 
  janitor::clean_names() %>% 
  select(date = date_time, year, month, day, total_precip_mm) %>% 
  mutate(
    yday = yday(date)
  )

write_csv(ott_weather, "day07 - physical/data/ott_precip.csv")

# Construction data -------------------------------------------------------

total_precip <-
  ott_weather %>% 
  group_by(year) %>% 
  summarise(
    total_yearly_precip = sum(total_precip_mm, na.rm = T),
    end_yday = 366,
    start_yday = 0,
    y = 0
    )

months <-
  tibble(
    month = str_to_lower(month.abb),
    month_start = yday(seq(as.Date("2021-01-01"), by = "month", length.out = 12)),
    label_position = month_start + 15,
    year = 2012
  ) %>% 
  mutate(month_start = ifelse(month_start == 1, NA, month_start))



# Plot construction -------------------------------------------------------

ott_weather %>% 
  ggplot(aes(yday, -total_precip_mm)) +
  geom_point(
    data = total_precip,
    aes(x = end_yday, y = y-10, size = total_yearly_precip),
   # alpha = 0.1,
    color = "white"
  ) +
  geom_col(aes(fill = -total_precip_mm)) +
  geom_text(
    data = total_precip,
    aes(x = start_yday-15, y = y, label = year),
    size = 20, 
    family = "opensanscond"
  ) +
  geom_text(
    data = months,
    aes(x = label_position, y = 15, label = month),
    size = 12, 
    family = "opensanscond"
  ) +
  geom_segment(
    data = months,
    aes(x = month_start, xend = month_start, y = 5, yend = 15),
    size = 0.25,
    color = "grey55"
  ) +
  geom_text(
    data = tibble(x = 315, y = -50, year = 2014),
    aes(x = x, y = y),
    label = "size of circles = total precipitation that year",
    family = "opensanscond",
    size = 8
  ) +
  geom_curve(
    data = tibble(x = 315, y = -60, year = 2014),
    aes(x = x, y = y, xend = 365, yend = -40),
    color = "grey55", 
    size = 0.25,
    arrow = arrow(length = unit(0.25, "lines"))
  ) +
  labs(
    title = "precipitation in ottawa",
    caption = "viz - @maiapelletier"
  ) +
  ylim(c(-80, 15)) +
  scale_size(range = c(6, 12)) +
  facet_wrap(year~., ncol = 1) +
  theme_void(base_family = "opensanscond") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey92", color = NA),
    plot.margin = margin(t = 10, l = 5, r = 5, b = 5),
    plot.title = element_text(hjust = 0.5, size = 160, margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0.5, size = 30),
    strip.text = element_blank(),
    strip.background = element_blank()
  ) +
  ggsave(here("day07 - physical", "day07_physical.png"), type = "cairo", dpi = 500, height = 8, width = 6)

