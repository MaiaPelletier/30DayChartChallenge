# day15 - multivariate ---------------------------------------------------------

library(tidyverse)
library(here)
library(ggforce)
library(gggibbous)

extrafont::loadfonts(device = "win")

# import data & create coordinates
yearly_snapshot <-
  read_csv(here("day15 - multivariate", "data", "yearly_snapshot.csv")) %>%
  mutate(
    hair_style = fct_inorder(hair_style),
    hair_colour = fct_inorder(hair_colour),
    x = rep(1:6, 4),
    y = c(rep(4, 6), rep(3, 6), rep(2, 6), rep(1, 6)),
    xmin = x - 0.1,
    ymin = y - 0.325,
    xmax = x + 0.3,
    ymax = y - 0.15,
    ratio = case_when(
      school == "elementary" ~ 0.25,
      school == "high school" ~ 0.45,
      school == "university" ~ 0.75,
      TRUE ~ 0
    )
  )

# for tattoo segments
tattoos <- 
  yearly_snapshot %>% 
  select(age, no_tattoos, x, y) %>% 
  filter(no_tattoos>0) %>% 
  mutate(
    xstart = x + 0.15,
    xend = x + 0.3, 
    yend = map(no_tattoos, function(x) seq(1.30, by = -0.05, length.out = x)),
    ystart = map(no_tattoos, function(x) seq(1.15, by = -0.05, length.out = x))
  ) %>% 
  unnest(c(ystart, yend))

# for point indicating canadian locations
locations <- 
  yearly_snapshot %>% 
    select(age, location, xmin, xmax, ymin, ymax) %>% 
    mutate(
      x = case_when(
        location %in% c("manitouwadge", "oakville") ~ xmin,
        location %in% c("stouffville", "ottawa") ~ xmax
      ),
      y = case_when(
        location %in% c("manitouwadge", "stouffville") ~ ymax,
        location %in% c("oakville", "ottawa") ~ ymin
      )
    ) %>% 
  select(age, location, x, y)


# build plot --------------------------------------------------------------

yearly_snapshot %>% 
  ggplot() +
  geom_rect(
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = country)
  ) +
  geom_point(
    data = locations,
    aes(x, y), 
    color = "#D5723C",
    size = 1
  ) +
  geom_circle(
    aes(x0 = x, y0 = y, r = 0.25), 
    color = "#34332F", 
   # fill = "#E9A7A8",
    size = 0.5
  ) +
  geom_moon(
    aes(x = x - 0.35, y = y, ratio = ratio),
    size = 4, 
    fill = NA,
    color = "#34332F"
    ) +
  geom_segment(
    data = tattoos,
    aes(x = xstart, y = ystart, xend = xend, yend = yend)
  ) +
  geom_point(
    aes(x = x, y = y + 0.35, shape = hair_style),
    size = 3,
    fill = "#34332F"
  ) +
  geom_text(
    aes(x, y, label = age),
    color = "#34332F", 
    size = 3,
    family = "Lora"
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 1.5, y = 5),
    label = "Yearly Snapshots", 
    size = 7,
    color = "#34332F",
    family = "Libre Caslon Display",
    fontface = "bold"
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 6.25, y = 5),
    label = "A snapshot of what my life looked like on each birthday.", 
    color = "#34332F",
    family = "Libre Caslon Display",
    hjust = 1
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 3.5, y = 0.25),
    label = "Viz - @MaiaPelletier", 
    color = "#34332F",
    family = "Libre Caslon Display",
    size = 2
  ) +
  scale_fill_manual(values = c("#456757", "#527883", "#AEC29B")) +
  scale_shape_manual(values = c(16, 18, 17)) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F5EFE1", color = "#34332F", size = 1),
    plot.margin = margin(20, 20, 0, 20),
    legend.position = "none"
  ) +
  ggsave(here("day15 - multivariate", "day15_multivariate.png"), type = "cairo", height = 7, width = 7)



# build legend ------------------------------------------------------------


legend <- 
  tibble(
    x = c(5.35, 5.35, 5.35, 4.9, 4.6),
    y = c(1.1, 0.75, 0.67, 1.35, 1),
    xend = c(5.5, 5.5, 5.5, 4.5, 4.5),
    yend = c(1.1, 0.75, 0.67, 1.35, 1),
    xtext = c(5.6, 5.6, 5.6, 4.4, 4.4),
    hjust = c(0, 0, 0, 1, 1),
    label = c("had this # of tattoos",
              "lived in this country",
              "lived in this city of that country",
              "wore my hair like this",
              "was at this level of education")
  )

#0.175
rect_legend <-
  yearly_snapshot %>% 
  filter(age %in% c(9, 15, 21)) %>%
  mutate(
    xmin = 4, 
    xmax = 4.4, 
    ymin = c(0.25, 0, -0.25), 
    ymax = ymin + 0.175, 
    xtext = xmax + 0.25, 
    ytext = ymax - 0.0875
    )

locations_legend <-
  yearly_snapshot %>% 
  filter(age %in% c(15)) %>% 
  mutate(
    country = "canada",
    xmin = 5, xmax = 5.4, ymin = 0, ymax = ymin + 0.175,
    x = map(xmin, function(x) c(rep(x, 2), rep(x + 0.4, 2))),
    y = map(ymin, function(x) rep(c(x, x + 0.175), 2))
  ) %>% 
    unnest(c("x", "y")) %>% 
    mutate(
      location = c("manitouwadge", "oakville", "ottawa", "stouffville"),
      vjust = c(1.5, -1.5, 1.5, -1.5)
      )

hair_points_legend <-
  yearly_snapshot %>% 
    distinct(hair_style) %>% 
    mutate(
      x = 5.75,
      y = rev(seq(0.25, 0.5, length.out = 3)) - 0.1
    )

moon_legend <-
  yearly_snapshot %>% 
  distinct(school, ratio) %>% 
  na.omit() %>% 
  mutate(
    x = 5.75,
    y = rev(seq(-0.25, 0, length.out = 3))
  )


yearly_snapshot %>% 
  filter(age == 23) %>% 
  ggplot() +
  geom_rect(
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = country)
  ) +
  geom_point(
    data = locations %>% filter(age == 23),
    aes(x, y), 
    color = "#D5723C",
    size = 1
  ) +
  geom_circle(
    aes(x0 = x, y0 = y, r = 0.25), 
    color = "#34332F",
    size = 0.5
  ) +
  geom_moon(
    aes(x = x - 0.35, y = y, ratio = ratio),
    size = 4, 
    fill = NA,
    color = "#34332F"
  ) +
  geom_segment(
    data = tattoos %>% filter(age == 23),
    aes(x = xstart, y = ystart, xend = xend, yend = yend)
  ) +
  geom_point(
    aes(x = x, y = y + 0.35, shape = hair_style),
    size = 3,
    fill = "#34332F"
  ) +
  geom_text(
    aes(x, y, label = age),
    color = "#34332F", 
    size = 4,
    family = "Lora"
  ) +
  geom_segment(
    data = legend,
    aes(x, xend = xend, y, yend = yend),
    color = "#34332F",
    size = 1
  ) +
  geom_text(
    data = legend,
    aes(x = xtext, y = yend, label = label, hjust = hjust), 
    size = 3,
    family = "Libre Caslon Display",
    color = "#34332F"
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 5, y = 1.75), 
    size = 6,
    color = "#34332F",
    label = "On this birthday I...",
    family = "Libre Caslon Display",
    fontface = "bold"
  ) +
  geom_rect(
    data = rect_legend,
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = country)
  ) +
  geom_text(
    data = rect_legend,
    aes(x = xtext, y = ytext, label = country),
    family = "Libre Caslon Display",
    size = 3,
    color = "#34332F"
  ) +
  geom_rect(
    data = locations_legend,
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = country)
  ) +
  geom_point(
    data = locations_legend,
    aes(x, y), 
    color = "#D5723C",
    size = 1
  ) +
  geom_text(
    data = locations_legend,
    aes(x, y, label = location, vjust = vjust), 
    size = 3,
    family = "Libre Caslon Display",
    color = "#34332F"
    ) +
   geom_point(
     data = hair_points_legend,
     aes(x, y, shape = hair_style),
     color = "#34332F",
     size = 3
   ) +
   geom_text(
     data = hair_points_legend,
     aes(x + 0.2, y, label = hair_style),
     size = 3,
     family = "Libre Caslon Display",
     color = "#34332F"
   ) +
  geom_moon(
    data = moon_legend,
    aes(x, y, ratio = ratio),
    size = 4, 
    fill = NA,
    color = "#34332F"
  ) +
  geom_text(
    data = moon_legend,
    aes(x + 0.2, y, label = school),
    size = 3, 
    family = "Libre Caslon Display",
    color = "#34332F"
  ) +
  geom_segment(
    data = data.frame(),
    aes(x = 4, xend = 6, y = 0.5, yend = 0.5),
    lty = 3, 
    size = 0.25, 
    color = "#34332F"
  ) +
  scale_fill_manual(values = c("#456757", "#527883", "#AEC29B")) +
  scale_shape_manual(values = c(18, 16, 17)) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F5EFE1", color = "#34332F", size = 1),
    #plot.margin = margin(t = 50, b = 50),
    #axis.text = element_text(),
    legend.position = "none"
  ) +
  ggsave(here("day15 - multivariate", "day15_multivariate_legend.png"), type = "cairo", dpi = 500)
  
