
# I'm Tired ---------------------------------------------------------------

library(tidyverse)
library(here)

data <- tibble(
  x = 1:10,
  y = 1:10
)

ggplot(data, aes(x, y)) +
  geom_line(size = 1) +
  labs(x = "Number of annotations a viz requires", y = "My readiness to give up on it") +
  theme_classic() +
  theme(
    plot.margin = margin(30, 30, 30, 30),
    axis.text = element_blank(),
    axis.title = element_text(size = 15)
  ) +
  ggsave(here("day12 - correlation", "imtired.png"), type = "cairo", height = 5, width = 5)
