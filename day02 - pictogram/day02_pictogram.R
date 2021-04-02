# day 02 - pictogram ---------------------------------------------------

library(tidyverse)
library(here)
library(ggimage)
library(showtext)

# Link to data from nookipedia
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

# data transformation -----------------------------------------------------

# data with the names of my villagers and their status (if they currently live on my island/if they have moved away)
my_villagers <- tribble(
  ~name,       ~status,
  "Anicotti",  "Moved Out",
  "Chevre",    "Moved Out",
  "Chrissy",   "Moved Out", 
  "Clay",      "Moved Out", 
  "Coco",      "Resident", 
  "Ellie",     "Resident", 
  "Flip",      "Moved Out", 
  "Grizzly",   "Resident", 
  "Gwen",      "Moved Out", 
  "Julia",     "Resident", 
  "Lopez",     "Moved Out", 
  "Lucky",     "Resident", 
  "Marshal",   "Resident", 
  "Molly",     "Resident", 
  "Ozzie",     "Moved Out", 
  "Piper",     "Moved Out", 
  "Puck",      "Moved Out", 
  "Rasher",    "Moved Out", 
  "Shep",      "Resident",
  "Sherb",     "Resident",
  "Sprinkle",  "Moved Out", 
  "Sylvia",    "Moved Out",
  "Wolfgang",  "Resident"
)

# get images for just my villagers and create axis positions for images
data <-
  villagers %>% 
  inner_join(my_villagers, by = "name") %>% 
  select(name, status, url) %>% 
  arrange(desc(status)) %>% 
  mutate(
    x = c(seq(1, 3), rep(seq(0,4), 4)),
    y = c(rep(5, 3), rep(4:1, each = 5)),
    status = factor(status, levels = c("Resident", "Moved Out"))
  ) 

# write data to a csv that i can link to in alt text so it can be explored by screen readers
write_csv(data, here("day02 - pictogram", "data", "my_animal_crossing_villagers.csv"))

# build plot --------------------------------------------------------------

font_add_google("Bubblegum Sans", "bubblegum")
showtext_auto()

ggplot(data) +
  geom_point(
    aes(x, y, shape = status),
    size = 20, stroke = 1, color = "#00AE77"
  ) +
  geom_image(
    aes(x = x, y = y, image = url),
    asp = 1.5, size = 0.075, by = "height"
    # asp = 1.5, size = 0.05, by = "height"
  ) +
  geom_text(
    aes(x = x, y = y + 0.5, label = name), 
    family = "bubblegum", size = 20, color = "white"
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 2, y = 0.25), label = "Viz: @MaiaPelletier",
    size = 15, color = "white", family = "bubblegum"
  ) +
  geom_label(
    data = data.frame(),
    aes(x = 2, y = 6.5), label = "My Animal Crossing Villagers",
    size = 60, fill = "#B07440", color = "#FFCF4B", family = "bubblegum"
  ) +
  scale_y_continuous(limits = c(0.25, 6.5)) +
  scale_shape_manual(
    values = c(16, 21),
    guide = guide_legend(title = NULL, override.aes = list(size = 2))
    ) +
  theme_void(base_family = "bubblegum") +
  theme(
    plot.margin = margin(10, 30, 0, 30),
    plot.background = element_rect(fill = "#C6E7E7", color = NA),
    legend.position = c(0.5, 0.875),
    legend.direction = "horizontal",
    legend.text = element_text(size = 40, color = "white")
  ) +
  ggsave(here("day02 - pictogram", "day02_pictogram.png"), type = "cairo", dpi = 500, height = 5.75, width = 8.5)


