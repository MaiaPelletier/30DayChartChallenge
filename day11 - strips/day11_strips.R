# day 11 - strips ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(showtext)
library(emojifont)

# data import -------------------------------------------------------------

ott_marriage <- read_excel(here("day11 - strips", "data", "Marriage_Death_Oath.xlsx"), sheet = 1)

write_excel_csv(ott_marriage, here("day11 - strips", "data", "MarriageLicenses.csv"))

# data transformation -----------------------------------------------------

ott_marriage_wide <-
  ott_marriage %>% 
  pivot_wider(
    names_from = Year,
    values_from = Total
  ) %>% 
  mutate(
    month_no = -row_number(),
    month_abb = month.abb
    )

max_marriage <- 
  ott_marriage_wide %>% 
  mutate(
    max = pmax(`2019`, `2020`),
    color = ifelse(max == `2019`, "2019", "2020"),
    max = ifelse(max == `2019`, -max - 150, max + 150),
    ) %>% 
  select(month_no, month_abb, max, color)


# build plot --------------------------------------------------------------

# font_add_google("Parisienne", "paris")
# font_add_google("Playfair Display", "playfair")

ott_marriage_wide %>% 
  ggplot() +
  geom_segment(
    aes(x = 0 + 75, xend = `2020` + 75, y = month_no, yend = month_no, color = "2020"),
    size = 1,
    lineend = "round"
  ) +
  geom_segment(
    aes(x = 0 - 75, xend = -`2019` - 75, y = month_no, yend = month_no, color = "2019"),
    size = 1,
    lineend = "round"
  ) +
  geom_text(
    aes(x = 0, y = month_no, label = paste(month_abb)),
    family = "paris",
    color = "#69636D",
    size = 14
    ) +
  geom_text(
    data = max_marriage,
    aes(x = max, y = month_no, color = color),
    label = fontawesome("fa-heart"),
    family = "fontawesome-webfont", 
    size = 10
  ) +
  geom_text(
    data = tibble(year = 2019:2020, x = c(-750, 750), y = c(-1.5, -1.5)),
    aes(x = x, y = y, label = year),
    size = 30,
    color = "#69636D",
    family = "playfair"
  ) +
  annotate(
    "text", 
    x = 0, y = 2,
    label = "Who's Getting Married?",
    family = "paris", 
    size = 60, 
    color = "#69636D"
    ) +
  annotate(
    "text", 
    x = 0, y = 0.5,
    label = "Total marriage licenses purchased in Ottawa",
    family = "playfair", 
    size = 16, 
    color = "#69636D"
  ) +
  annotate(
    "text", 
    x = 0, y = -13,
    label = "Viz - @MaiaPelletier | Data - Open Ottawa",
    family = "playfair", 
    size = 8, 
    color = "#69636D"
  ) +
  ### Annotations ###
  annotate(
    "text", 
    x = 700, y = -11,
    label = "Heart = year with the max",
    family = "playfair", 
    size = 8, 
    color = "#69636D"
  ) +
  annotate(
    "curve", 
    x = 700, y = -11.5, xend = 370, yend = -12,
    curvature = -0.35,
    size = 0.25,
    arrow = arrow(length = unit(0.25, "lines")),
    color = "#69636D"
  ) +
  annotate(
    "text", 
    x = 450, y = -5,
    label = "COVID-19 lockdowns begin",
    family = "playfair", 
    size = 8, 
    color = "#69636D"
  ) +
  annotate(
    "curve", 
    x = 400, y = -4.75, xend = 150, yend = -4,
    curvature = 0.35,
    size = 0.25,
    color = "#69636D",
    arrow = arrow(length = unit(0.25, "lines"))
  ) +
  annotate(
    "text", 
    x = -800, y = -10.5,
    label = "Max licenses in 1 month = 821",
    family = "playfair", 
    size = 8, 
    color = "#69636D"
  ) +
  annotate(
    "curve", 
    x = -850, y = -10, xend = -875, yend = -7.25,
    curvature = -0.5,
    size = 0.25,
    color = "#69636D",
    arrow = arrow(length = unit(0.25, "lines"))
  ) +
  xlim(c(-1000, 1000)) +
  ylim(c(-13, 2)) +
  scale_color_manual(values = c("#A3B466", "#F3979A")) +
  theme_void(base_family = "paris") +
  theme(
    plot.background = element_rect(fill = "#FFFBF8", color = NA),
    plot.margin = margin(t = 20),
    legend.position = "none"
  ) +
  ggsave(here("day11 - strips", "day11_strips.png"), type = "cairo", dpi = 500, height = 6, width = 5)
