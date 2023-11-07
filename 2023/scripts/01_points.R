# 30 Day Map Challenge, Day 1 - Points ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)

# Fonts
font_add_google("Outfit")
font_add_google("JetBrains Mono")
showtext_auto()

# Data
worldCities <- read_csv(here::here("2023/data/01_points_worldcities.csv")) %>%
  filter(capital == "primary")

world <- rnaturalearthdata::countries50 %>%
  sf::st_as_sf()

marathonData <- read_csv(here::here("2023/data/01_points.csv")) %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  left_join(worldCities, by = c("country" = "country")) %>%
  select(country, n, lat, lng) %>%
  filter(n >= 10) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs = sf::st_crs(world))

# Map ####
pointsMap <- ggplot(marathonData) +
  geom_sf(data = world, linewidth = 0.05, fill = "grey99", colour = "grey75") +
  geom_sf(aes(size = n, colour = country, geometry = geometry),  alpha = 0.75) +
  ggrepel::geom_text_repel(data = marathonData %>% filter(n >= 20), 
                           aes(label = paste0(country, "\n", n, " Winners"), geometry = geometry),
                           stat = "sf_coordinates",
                           max.overlaps = 20, colour = "#000000", family = "JetBrains Mono", 
                           size = 14, lineheight = 0.3) +
  annotate("text", x = 6.9e5, y = -9.4e6, label = "Marathon Winners\nby Country", size = 35, 
           fontface = "bold", family = "Outfit", lineheight = 0.3) +
  annotate("text", x = 6.9e5, y = -1.15e7, label = "#30DayMapChallenge | Day 1: Points | @hdailey\nSource: Kaggle.com",
           size = 6, colour = "grey55", family = "JetBrains Mono", lineheight = 0.3) +
  coord_sf(crs = "+proj=adams_ws1", expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey95", colour = NA))

# Save ####
ggsave(plot = pointsMap, path = here::here("2023/maps/"), "01_points.png", dpi = 320, height = 8, width = 11, unit = "in")
