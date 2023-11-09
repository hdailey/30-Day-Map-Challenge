# 30 Day Map Challenge, Day 9 - Hexagons ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(showtext)
library(sysfonts)

# Fonts
font_add_google("Big Shoulders Display", db_cache = FALSE)
showtext_auto()

# Data
spdfUS <- geojson_read(here::here("30-Day-Map-Challenge/2023/data/09_hexagon.geojson"), 
                       what = "sp")
dataCDC <- read.csv(here::here("30-Day-Map-Challenge/2023/data/09_hexagon_CDCdata.csv"))

spdfUS@data = spdfUS@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdfUS_fortified <- sf::st_as_sf(spdfUS, region = "google_name") %>%
  left_join(dataCDC, by = c("iso3166_2" = "STATE"))
hexCentroid <- cbind.data.frame(data.frame(gCentroid(spdfUS, byid = TRUE), 
                                           id = spdfUS@data$iso3166_2))
# Map ####
plotHex <- ggplot() +
  geom_sf(data = spdfUS_fortified, aes(geometry = geometry, fill = RATE), color="#FFFFFF") +
  geom_text(data = hexCentroid, aes(x = x, y = y, label = id), family = "Big Shoulders Display", 
            colour = "#FFFFFF", fontface = "bold", size = 18) +
  coord_sf() +
  scale_fill_viridis_c(option = "F") +
  labs(title = "Heart Disease Mortality (Age Adjusted)",
       caption = "Source: CDC.gov & @andrewxhill | #30DayMapChallenge | Day 9 - Hexagons | @hdailey\nInspired by @andrewxhill") +
  theme_void() +
  theme(text = element_text(family = "Big Shoulders Display"),
        plot.title = element_text(family = "Big Shoulders Display", face = "bold", hjust = 0.5, size = 72,
                                  margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0.5, size = 40, colour = "grey35", lineheight = 0.3),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.text = element_text(size = 42, hjust = 1, vjust = 1, angle = 45),
        legend.margin = margin(b = -75),
        plot.margin = margin(5, 2, 1, 2),
        plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF")) +
  guides(fill = guide_colourbar(title = '', title.justification = 'center', label.position = 'bottom',
                                barwidth = 10, barheight = 2))

# Save ####
ggsave(plot = plotHex, path = here::here("30-Day-Map-Challenge/2023/maps/"), "09_hexagons.png", dpi = 320, height = 8, width = 11, unit = "in")
