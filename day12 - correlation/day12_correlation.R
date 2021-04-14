# day 12 - correlation ---------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(ggimage)
library(magick)

extrafont::loadfonts(device = "win")

# import data -------------------------------------------------------------

wingspan_data <- 
  read_excel(here("day12 - correlation", "data", "wingspan-card-lists.xlsx"), sheet = "Core") %>% 
  janitor::clean_names() %>% 
  select(common_name, victory_points, wingspan) %>% 
  slice(-c(1:2)) %>% 
  arrange(victory_points, wingspan)

write_excel_csv(wingspan_data, here("day12 - correlation", "data", "wingspan_data.csv"))

# build plot --------------------------------------------------------------

highlighted_birds <- 
  c("Baltimore Oriole", "California Condor", "Black-Chinned Hummingbird", "Mallard")

background <- magick::image_read(here("day12 - correlation/images/wing-bg.png"))

ggplot(wingspan_data, aes(x = victory_points, y = -wingspan)) +
  background_image(background) +
  geom_segment(
    aes(xend = victory_points, y = 0, yend = -max(wingspan)),
    size = 0.1, color = "grey80", alpha = 0.25
  ) +
  geom_point(
    data = distinct(wingspan_data, victory_points),
    aes(x = victory_points, y = 20),
    color = "grey75",
    size = 7, 
    alpha = 0.75
  ) +
  geom_text(
    data = distinct(wingspan_data, victory_points),
    aes(x = victory_points, y = 20, label = victory_points),
    family = "Lato",
    color = "white",
    size = 3
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 4.5, y = 50),
    label = "Victory Points",
    family = "Lato", 
    size = 3, 
    color = "white"
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 4.5, y = -350), label = "WINGSPAN",
    size = 24,
    family = "Lato", 
    color = "white"
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 4.5, y = -390), label = "There is a slight positive relationship between increasing wingspan size & how many victory points a bird is worth in the game.",
    size = 2.5,
    family = "Lato", 
    color = "white"
  ) +
  geom_point(
    data = wingspan_data %>% filter(common_name %in% highlighted_birds),
    size = 2, color = "#F0A37F"
  ) +
  ggbeeswarm::geom_quasirandom(
    data = wingspan_data %>% filter(!common_name %in% highlighted_birds),
    size = 0.75, color = "grey40", shape = 21
    ) +
  ## Annotations ##
  annotate(
    "text",
    x = 0,
    y = -100,
    label = "Mallard\n89cm",
    size = 1.5,
    family = "Lato"
  ) +
  annotate(
    "text",
    x = 1,
    y = -260,
    label = "California Condor\n277cm",
    size = 1.5,
    family = "Lato"
  ) +
  annotate(
    "text",
    x = 5,
    y = -2,
    label = "Black-Chinned Hummingbird\n8cm",
    size = 1.5,
    family = "Lato"
  ) +
  annotate(
    "text",
    x = 9.5,
    y = -45,
    label = "Baltimore Oriole\n30cm",
    size = 1.5,
    family = "Lato"
  ) +
  xlim(c(-1, 10)) +
  ylim(c(-400, 50)) +
  theme_void() +
  ggsave(here("day12 - correlation", "day12_correlation.png"), type = "cairo", dpi = 500, height = 6, width = 6)


