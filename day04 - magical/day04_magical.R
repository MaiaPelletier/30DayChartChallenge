# day 04 - magical ---------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(ggforce)
library(gggibbous)

extrafont::loadfonts(device="win")

fav_fantasy <- 
  read_csv(here("day04 - magical", "data", "my_fav_fantasy_novels.csv")) %>% 
  clean_names() %>% 
  select(title, authors, dates_read, star_rating) %>% 
  mutate(
    ratio = star_rating/5,
    a = seq(0, 315, 35),
    t = rep(c(seq(0, 70, 35), 285, 320), 2),
    x = cos(a * pi / 180),
    y = sin(a * pi / 180),
    align = c(rep("left", 3), rep("right", 5), rep("left", 2))
    )

fav_fantasy %>% 
  ggplot(aes(x, y)) +
  geom_moon(ratio = 1, fill = "grey35", right = FALSE, size = 10) +
  geom_moon(aes(ratio = ratio), fill = "white", right = FALSE, size = 10, color = "grey35") +
  geom_text(
    aes(x = x * 1.5, y = y * 1.5, angle = t, label = paste0(title, "\n", authors), hjust = align),
    size = 1.5, color = "grey80", family = "Perpetua Titling MT"
    ) +
  annotate(
    "text", 
    label = "My\nFavourite\nFantasy\nNovels", x = 0, y = 0,
    color = "white", family = "Perpetua Titling MT", face = "bold", size = 4
    ) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(100, 100, 100, 100)
  ) +
  ggsave(here("day04 - magical", "day04_magical.png"), type = "cairo", dpi = 500, height = 7, width = 7)
  


  