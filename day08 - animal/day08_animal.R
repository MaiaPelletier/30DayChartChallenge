# day 07 - physical ---------------------------------------------------

library(tidyverse)
library(here)
library(ggridges)
library(lubridate)

extrafont::loadfonts(device = "win")


# data import -------------------------------------------------------------

my_ebird_data <- 
  read_csv(here("day08 - animal/raw data/MyEBirdData.csv")) %>% 
  janitor::clean_names()


# data manip --------------------------------------------------------------

birding_months <- 
  my_ebird_data %>%
  add_count(common_name) %>% 
  filter(count != "X", n >= 5) %>%
  select(common_name, count, date) %>% 
  mutate(
    month = month(date)
  ) %>% 
  group_by(common_name, month) %>% 
  summarise(sightings = sum(as.numeric(count))) %>% 
  mutate(
    total_sightings = sum(sightings),
    pct = sightings/total_sightings,
    season = case_when(
      month %in% 3:5 ~ "Spring",
      month %in% 6:8 ~ "Summer",
      month %in% 9:11 ~ "Fall",
      TRUE ~ "Winter"
    ), 
    season = fct_reorder(season, month),
    common_name = fct_reorder(common_name, sightings)
  )

# write data for screenreaders
birding_months %>% 
  left_join(tibble(month = 1:12, month_name = month.name)) %>% 
  select(common_name, season, month_name, sightings, pct_species = pct) %>% 
  write_csv(here("day08 - animal", "data", "birding_thru_the_months.csv"))


# build plot --------------------------------------------------------------

p <-
  birding_months %>% 
  ggplot(aes(month, pct)) +
  geom_point(
    aes(x = 6.5, y = 1, size = total_sightings), 
    color = "#F9D4C8",
    show.legend = FALSE
  ) +
  geom_col(aes(fill = season), width = 0.7) +
  geom_text(
    data = tibble(x = 10.5, y = 0.3, common_name = "American Black Duck"),
    aes(x, y, label = "Size =\n# individual\nsightings"),
    size = 1.5, family = "Lato", color = "grey60", fill = NA
  ) +
  geom_curve(
    data = tibble(x = 8.5, y = 0.3, common_name = "American Black Duck"),
    aes(x, xend = 6.5, y, yend = 0.8),
    size = 0.25, color = "grey60", curvature = -0.35,
    arrow = arrow(length = unit(0.15, "lines"))
  ) +
  labs(
    title = "Birding By the Months",
    subtitle = "The percent of the total of a species of bird I observed (i.e. submitted to eBird) over the months in a year.\nI've never seen a single bird in the summer apparently.",
    caption = "Viz - @MaiaPelletier"
  ) +
  facet_wrap(common_name~., ncol = 5, nrow = 7) +
  ylim(c(0, 1.3)) +
  scale_fill_manual(
    values = c("#92D0DC", "#389064", "#E47780", "#E2424C"),
    guide = guide_legend(title = NULL)
    ) +
  scale_size(range = c(3, 9)) +
  theme_void(base_family = "Montserrat") +
  theme(
    plot.background = element_rect(fill = "#FDF1ED", color = NA),
    plot.margin = margin(10, 20, 10, 20),
    plot.title = element_text(hjust = 0.5, size = 30, family = "Montserrat Medium", color = "grey30"),
    plot.subtitle = element_text(hjust = 0.5, size = 6, family = "Lato", color = "grey30", margin = margin(t = 5)),
    plot.caption = element_text(hjust = 0.5, size = 6, margin = margin(t = 15), color = "grey30"),
    legend.position = "top",
    legend.margin = margin(t = 10, b = 10),
    legend.key.width = unit(7, "points"),
    legend.key.height = unit(7, "points"),
    legend.text = element_text(size = 6, color = "grey30"),
    strip.text = element_text(size = 6, margin = margin(t = 2, b = 2), color = "grey30")
  ) +
  ggsave(here("day08 - animal", "day08_animal.png"), type = "cairo", dpi = 500, height = 8, width = 7)