# day17 - pop culture ---------------------------------------------------------

library(tidyverse)
library(here)
library(rvest)
library(ragg)
library(ggrepel)

extrafont::loadfonts(device = "win")


# Scrape data from wikipedia ----------------------------------------------

wes_anderson_wiki <- read_html("https://en.wikipedia.org/wiki/Filmography_of_Wes_Anderson")

wes_anderson_movies <- 
  wes_anderson_wiki %>% 
  html_element(".unsortable:nth-child(20)") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(year, film, rotten_tomatoes = rotten_tomatoes_2, box_office = box_office_4) %>% 
  slice(-nrow(.)) %>% 
  mutate(
    rotten_tomatoes = as.numeric(str_extract(rotten_tomatoes, "\\d{2}"))/100,
    box_office = parse_number(box_office),
    box_office = ifelse(year == 1996, box_office/1000000, box_office)
  )

write_csv(wes_anderson_movies, here("day17 - pop culture", "data", "wes_anderson_movies.csv"))

# Some data manipulation for the plot -------------------------------------

wes_summary_stats <- 
  wes_anderson_movies %>% 
    summarise_if(is.numeric, mean)

quadrant_labels <- tibble(
  label = c("Flop (by Anderson standards)", "Under-rated masterpiece", "Well-recognized treasure", "Over-rated/Over-hyped"),
  x = c(15, 15, 155, 155),
  y = c(0.5, 1, 1, 0.5)
)

wes_anderson_movies_quads <- 
  wes_anderson_movies %>% 
  mutate(
    quad = case_when(
      box_office > wes_summary_stats$box_office & rotten_tomatoes > wes_summary_stats$rotten_tomatoes ~ 1,
      box_office < wes_summary_stats$box_office & rotten_tomatoes > wes_summary_stats$rotten_tomatoes ~ 2,
      box_office < wes_summary_stats$box_office & rotten_tomatoes < wes_summary_stats$rotten_tomatoes ~ 3,
      box_office > wes_summary_stats$box_office & rotten_tomatoes < wes_summary_stats$rotten_tomatoes ~ 4
    ),
    quad = fct_inseq(factor(quad))
  )


# Build plot --------------------------------------------------------------

wa_pal <- c("#EA839E", "#B1758E", "#5D7C98", "#87839E")


ggplot(wes_anderson_movies_quads, aes(box_office, rotten_tomatoes)) +
  geom_text_repel(
    aes(label = paste0(film, " (", year, ")")),
    seed = 69,
    size = 1.75,
    segment.size = 0.25,
    family = "Lora",
    box.padding = 0.4,
    color = "#2F232B",
    direction = "both",
    force = 10
  ) +
  geom_point(
    aes(color = quad),
    size = 5
    ) +
  geom_point(
    color = "white",
    size = 1
  ) +
  geom_hline(
    data = wes_summary_stats,
    aes(yintercept = rotten_tomatoes), 
    color = "#6A4B61",
    lty = 3
  ) +
  geom_vline(
    data = wes_summary_stats,
    aes(xintercept = box_office), 
    color = "#6A4B61",
    lty = 3
  ) +
  geom_label(
    data = quadrant_labels,
    aes(x, y, label = label),
    family = "Lora",
    fontface = "bold",
    size = 2.5, 
    fill = "#6A4B61", 
    color = "#EBD6DB",
    label.r = unit(0.75, "lines"),
    label.padding = unit(0.45, "lines")
  ) +
  annotate("text",
    x = 155, y = wes_summary_stats$rotten_tomatoes-0.01,
    label = "MEAN ROTTEN TOMATOES RATING",
    size = 2,
    color = "#6A4B61",
    family = "Lora"
  ) +
  annotate("text",
    x = wes_summary_stats$box_office + 5, y = 0.54,
    label = "MEAN BOX OFFICE SALES",
    angle = 90,
    size = 2,
    color = "#6A4B61",
    family = "Lora"
  ) +
  labs(
    title = "WES ANDERSON'S FILMOGRAPHY",
    caption = "Viz - @MaiaPelletier"
  ) +
  scale_x_continuous(
    breaks = seq(0, 150, 50),
    labels = paste0("$", seq(0, 150, 50), "M"),
    limits = c(-10, 175)
    ) +
  scale_y_continuous(
    breaks = seq(0.6, 1, 0.2),
    labels = scales::percent_format(),
    limits = c(0.5, 1)
    ) +
  scale_color_manual(values = wa_pal) +
  theme_void(base_family = "Lora", base_size = 8) +
  theme(
    plot.background = element_rect(fill = "#EBD6DB", color = "#6A4B61", size = 2),
    plot.margin = margin(20, 40, 10, 20),
    plot.title = element_text(hjust = 0.5, size = 26, color = "#2F232B", margin = margin(b = 10), face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = 6, color = "#2F232B", margin = margin(t = 10)),
    legend.position = "none",
    axis.text = element_text(color = "#2F232B", margin = margin(t = 10, r = 10))
  ) +
  ggsave(here("day17 - pop culture", "day17_popculture.png"), dpi = 600, height = 5.5, width = 7)

