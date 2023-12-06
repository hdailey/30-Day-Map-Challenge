# 30 Day Map Challenge, Day 2 - Lines ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)
library(osmdata)
library(sf)
library(wesanderson)
library(showtext)

# Fonts
font_add_google("Maven Pro")
showtext_auto()

# Data
Roads <- opq(bbox = c(-121.5919973, 38.6938226, -121.3460930, 38.4287675)) %>%
  add_osm_features(c("highway" = "primary",
                   "highway" = "secondary",
                   "highway" = "tertiary",
                   "highway" = "motorway",
                   "highway" = "residential",
                   "highway" = "road")) %>%
  osmdata_sf() %>% 
  .$osm_lines %>%
  select(geometry)

Neighborhoods <- opq(bbox = c(-121.5919973, 38.6938226, -121.3460930, 38.4287675)) %>%
  add_osm_features(c("place" = "suburb")) %>%
  osmdata_sf() %>% 
  .$osm_polygons

sacData <- data.frame()
  for(i in 1:nrow(Neighborhoods)){
    Int_Neighborhoods <- st_intersection(Roads, Neighborhoods[i, ])
    sacData <- rbind(sacData, Int_Neighborhoods)
  }

# Map ####
pal <- colorRampPalette(wes_palette("Zissou1", n = 5, type = "continuous"), 19)

linesMap <- ggplot() +
  geom_sf(data = Roads, aes(geometry = geometry), linewidth = 0.1, colour = "grey95") +
  geom_sf(data = sacData, aes(geometry = geometry, colour = name), linewidth = 0.15,
          show.legend = FALSE) +
  ggrepel::geom_label_repel(data = unique(Neighborhoods),
                   aes(label = name, geometry = geometry, colour = name), fill = "grey5",
                   size = 4, label.padding = unit(0.1, "cm"),
                   fontface = "bold", stat = "sf_coordinates",
                   show.legend = FALSE, family = "Maven Pro") +
  scale_colour_manual(palette = pal) +
  theme_void() +
  labs(title = "Roads and Neighborhoods of Sacramento",
       caption = "Source: OpenStreetMaps | #30DayMapChallenge | Day 2: Lines | @hdailey") +
  theme(text = element_text(family = "Maven Pro"),
        plot.title = element_text(face = "bold", colour = "#EBCC2A", hjust = 0.5, size = 24),
        plot.caption = element_text(colour = "#EBCC2A", hjust = 0.5, size = 16,
                                    margin = margin(b = 5)),
        
        plot.background = element_rect(fill = "grey5", colour = "grey5"))
# Save ####
ggsave(plot = linesMap, path = here::here("2023/maps/"), "02_lines.png", height = 4, width = 3, units = "in", dpi = 320)
