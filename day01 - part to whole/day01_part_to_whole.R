
# day 01- part to whole ---------------------------------------------------
# daily journal

library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(showtext)

# Load data ---------------------------------------------------------------

daily_sched <- read_csv(here("day01 - part to whole", "data", "dailysched_31march2021.csv"))

# Transform data ----------------------------------------------------------

daily_sched_transf <-
  daily_sched %>% 
    mutate(
      duration = difftime(end_time, start_time, units = "mins"),
      duration_num = as.numeric(duration),
      category = fct_inorder(factor(category))
    )

# Build plot --------------------------------------------------------------

palette <- c(
  "#FF0000",
  "#FF7070",
  "#F09200",
  "#FFBF1F",
  "#00A08A",
  "#2989A3",
  "#5BBCD6",
  "#A475D9"
)

font_add_google("Cedarville Cursive", "cedarville")

showtext_auto()

daily_sched_transf %>% 
  mutate(start_time = as.POSIXct(start_time)) %>% 
  ggplot() +
  geom_rect(
    aes(xmin = start_time, xmax = start_time + duration, ymin = 0, ymax = 1, fill = category),
    color = "grey30", 
    size = 0.25
    ) + 
  labs(
    title = "Daily Journal - March 31, 2021",
    caption = "Viz - @MaiaPelletier"
  ) +
  scale_fill_manual(
    values = palette,
    guide = guide_legend(title = NULL)
    ) +
  scale_x_datetime(
    breaks = scales::date_breaks("4 hour"), date_labels = "%l %p"
    ) +
  theme_void(base_family = "cedarville") +
  theme(
    plot.background = element_rect(fill = "#FCEBDA", color = NA),
    plot.margin = margin(10, 10, 5, 10),
    plot.title = element_text(size = 56, hjust = 0.5, margin = margin(t = 5, b = 10)),
    plot.caption = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 20, vjust = 2),
    legend.position = "top",
    legend.text = element_text(size = 20),
    legend.key.height = unit(0.75, "lines"),
    legend.margin = margin(b = 5)
  ) +
  ggsave(here("day01 - part to whole", "day01_part_to_whole.png"), type = "cairo")


