# 30 Day Map Challenge, Day 6 - Asia ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)
library(osmdata)
library(sf)
library(showtext)
library(sysfonts)

# Fonts
font_add_google("Satisfy", db_cache = FALSE)
font_add_google("Advent Pro", db_cache = FALSE)
showtext_auto()

# Data
bhutan <- st_read(here::here("2023/data/2023-11-06/btn_admbnda_adm0_bnlc_20201026.shp"),
                  crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

bhutanRoads <- opq(bbox = c(88.74604, 26.7006, 92.12475, 28.24768)) %>%
  add_osm_features(features = list("highway" = "primary", "highway" = "secondary",
                                   "highway" = "tertiary", "highway" = "motorway",
                                   "highway" = "residential", "highway" = "road")) %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  select(geometry) %>%
  st_intersection(bhutan)

bhutanGlaciers <- opq(bbox = c(88.74604, 26.7006, 92.12475, 28.24768)) %>%
  add_osm_features(features = list("natural" = "glacier")) %>%
  osmdata_sf() %>%
  .$osm_polygons %>%
  select(geometry) %>%
  st_intersection(bhutan)

bhutanBareRock <- opq(bbox = c(88.74604, 26.7006, 92.12475, 28.24768)) %>%
  add_osm_features(features = list("natural" = "bare_rock")) %>%
  osmdata_sf() %>%
  .$osm_polygon %>%
  select(geometry) %>%
  st_intersection(bhutan)

bhutanTrees <- opq(bbox = c(88.74604, 26.7006, 92.12475, 28.24768)) %>%
  add_osm_features(features = list("natural" = "wood")) %>%
  osmdata_sf() %>%
  .$osm_polygon %>%
  select(geometry) %>%
  st_intersection(bhutan)

# Map ####
bhutanMap <- ggplot() +
  geom_sf(data = bhutanTrees, aes(), fill = "lightgreen", colour = NA) +
  geom_sf(data = bhutanBareRock, aes(), fill = "grey", colour = NA) +
  geom_sf(data = bhutanGlaciers, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = bhutanRoads, aes(), colour = "#F16666", linewidth = 0.35) +
  theme_void() +
  labs(title = "Bhutan",
       caption = "#30DayMapChallenge | Day 6: Asia | @hdailey <br> Source: Humanitarian Data Exchange and Open Street Maps") +
  theme(text = element_text(family = "Advent Pro"),
        plot.title = element_text(family = "Satisfy", face = "bold", hjust = 0.5, size = 72, 
                                  margin = margin(t = 2, b = 0)),
        plot.title.position = "plot",
        plot.caption = ggtext::element_textbox_simple(hjust = 0.5, halign = 0.5, size = 40, 
                                                      margin = margin(t = 1, b = 2),
                                                      lineheight = 0.4),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "grey95", colour = NA))

# Save ####
ggsave(plot = bhutanMap, path = here::here("2023/maps/"), "06_asia.png", dpi = 320, height = 8, width = 11, unit = "in")
