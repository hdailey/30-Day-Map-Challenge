# 30 Day Map Challenge, Day 3 - Polygons ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)
library(sf)
library(showtext)

# Fonts
font_add_google("Hedvig Letters Serif", db_cache = FALSE)
showtext_auto()

# Data
votingData <- read_sf(dsn = here::here("2023/data/03_polygons/"))


# Map ####
polygonMap <- ggplot() +
  geom_sf(data = votingData, aes(geometry = geometry, fill = PREC_KEY), show.legend = FALSE) +
  labs(title = "California's Voting Districts",
       caption = "Source: statewidedatabase.org | #30DayChartChallenge | Day 3: Polygons | @hdailey") +
  theme_void() +
  theme(text = element_text(family = "Hedvig Letters Serif"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 48),
        plot.caption = element_text(hjust = 0.5, margin = margin(b = 5), size = 20),
        plot.background = element_rect(fill = "grey95", colour = "grey95"))

# Save ####
ggsave(plot = polygonMap, path = here::here("2023/maps/"), "03_polygons.png", height = 6, width = 5, units = "in", dpi = 320)
