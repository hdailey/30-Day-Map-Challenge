# 30 Day Map Challenge, Day 21 - Raster ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)
library(rnaturalearth)
library(sf)
library(raster)
library(viridis)
library(showtext)

# Fonts
font_add_google("Smooch Sans")
font_add_google("Noto Sans")
showtext_auto()

# Data
california <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "California")

californiaDEM <- elevatr::get_elev_raster(locations = california, z = 9, clip = "locations",
                                          neg_to_na = TRUE)
californiaDEM_df <- as.data.frame(californiaDEM, xy = TRUE) %>%
  rename("elevation" = "file52203453185a",
         "lat" = "y",
         "long" = "x") %>%
  na.omit(elevation)

# Map ####
mapRaster <- californiaDEM_df %>%
  ggplot() +
  geom_sf(data = california, size = 0.2, color = "#000000", fill = NA) +
  geom_raster(data = californiaDEM_df, aes(x = long, y = lat, fill = elevation)) +
  geom_contour(data = californiaDEM_df, aes(x = long, y = lat, z = elevation), color = "#F9F9F9", linewidth = 0.1) +
  # Mount Shasta and Shastina
  annotate("segment", x = -122.193, y = 41.409, xend = -119.5, yend = 41.409, linewidth = 0.3, color = "#FFFFFF") +
  annotate("text", x = -119, y = 41.409, label = "Mount Shasta\n4,317 meters\nand\nShastina\n3,758 meters",
           hjust = "center", family = "Smooch Sans", size = 10, color = "navyblue",
           lineheight = 0.2) +
  # Lassen
  annotate("segment", x = -121.508, y = 40.492, xend = -119.5, yend = 40.492, linewidth = 0.3, color = "#FFFFFF") +
  annotate("text", x = -119, y = 40.492, label = "Mount Lassen\n3,189 meters",
           hjust = "center", family = "Smooch Sans", size = 10, color = "navyblue",
           lineheight = 0.2) +
  # Title
  annotate("text", x = -116, y = 39, label = "California's Volcanoes\n3 volcanoes in California are greater than\n3000 meters in elevation", hjust = "center",
           family = "Smooch Sans", size = 16, colour = "#F9F9F9", lineheight = 0.3) +
    scale_fill_viridis(option = "G") +
  coord_sf() +
  theme_void() +
  theme(text = element_text(colour = "#F9F9F9"),
        plot.background = element_rect(fill = colorspace::lighten("grey25", 0.2), 
                                       colour = colorspace::lighten("grey25", 0.2)),
        plot.title = element_text(family = "Noto Sans", face = "bold"),
        legend.position = c(0.65, 0.6),
        legend.direction = "horizontal")
  

# Save ####
ggsave(plot = mapRaster, path = here::here("2023/maps/"), "21_raster.png", dpi = 320)
