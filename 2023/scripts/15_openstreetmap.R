# 30 Day Map Challenge, Day 15 - OpenStreetMap ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)
library(showtext)
library(osmdata)
library(sf)
library(mapview)
library(ggtext)
# Fonts
font_add_google("Roboto Condensed", db_cache = FALSE)
font_add_google("Oswald", db_cache = FALSE)
showtext_auto()

# Function from https://www.mzes.uni-mannheim.de/ -----------------------------
rotate_sf <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function (x) { 
    matrix(c(2, 1.2, 0, 1), 2, 2) 
  }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  
  data %>% 
    dplyr::mutate(
      geometry = 
        .$geometry * shear_matrix() * rotate_matrix(pi / 20) + c(x_add, y_add)
    )
}

# Data
sf_use_s2(FALSE)
ucsc <- st_read(here::here("2023/data/15_openstreetmaps/CollegeUniversityCampuses.shp")) %>%
  st_transform("+proj=longlat +datum=WGS84")

## Roads
ucscRoads <- opq(bbox = c(-122.0868, 36.97711 , -122.0475, 37.0243)) %>%
  add_osm_feature("highway") %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  select(highway, geometry) %>%
  st_collection_extract(type = 'LINESTRING') %>%
  filter(!is.na(highway)) %>%
  st_intersection(ucsc) %>%
  select(geometry)

ucscRoadsPoly <- opq(bbox = c(-122.0868, 36.97711 , -122.0475, 37.0243)) %>%
  add_osm_feature("highway") %>% 
  osmdata_sf() %>% 
  .$osm_multipolygons %>% 
  st_intersection(ucsc) %>% 
  select(geometry)


##Greenspace
ucscGreenspace <- opq(bbox = c(-122.0868, 36.97711, -122.0475, 37.0243), timeout = 25000) %>%
  add_osm_features(features = list("landuse" = "grass",
                                   "landuse" = "farmland",
                                   "landuse" = "forest",
                                   "landuse" = "meadow",
                                   "landuse" = "recreation_ground",
                                   "leisure" = "common",
                                   "leisure" = "commons",
                                   "leisure" = "garden",
                                   "leisure" = "park",
                                   "leisure" = "playground"
                                   )) %>%
  osmdata_sf() %>%
  .$osm_polygons %>% 
  select(geometry) %>%
  st_intersection(ucsc)

ucscGreenspace_trees <- opq(bbox = c(-122.0868, 36.97711 , -122.0475, 37.0243)) %>%
  add_osm_features(features = list("natural" = "tree")) %>% 
  osmdata_sf() %>% 
  .$osm_points %>% 
  st_intersection(ucsc) %>% 
  select(geometry)

## Buildings
ucscBuildings <- opq(bbox = c(-122.0868, 36.97711 , -122.0475, 37.0243)) %>%
  add_osm_feature("building") %>%
  osmdata_sf() %>%
  .$osm_polygons %>%
  st_intersection(ucsc) %>%
  select(geometry)

ucscBuildings_mps <- opq(bbox = c(-122.0868, 36.97711 , -122.0475, 37.0243)) %>%
  add_osm_feature("building") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  st_intersection(ucsc) %>%
  select(geometry)

# Map ####
map3d <- ggplot() +
  #Roads
  geom_sf(data = rotate_sf(ucsc), aes(geometry = geometry), 
          fill = "grey", colour = "grey5", alpha = 0.2) +
  geom_sf(data = rotate_sf(ucscRoads), aes(geometry = geometry), 
          colour = "grey25", linewidth = 0.05, alpha = 0.5) +
  geom_sf(data = rotate_sf(ucscRoadsPoly), aes(geometry = geometry), 
          fill = "grey25", colour = NA, alpha = 0.5) +
  #Buildings
  geom_sf(data = rotate_sf(ucsc, y_add = 0.05), aes(geometry = geometry),
          fill = "gold", colour = "grey5", alpha = 0.2) +
  geom_sf(data = rotate_sf(ucscBuildings, y_add = 0.05), aes(geometry = geometry), 
          fill = "goldenrod4", colour = "goldenrod", alpha = 0.5) +
  geom_sf(data = rotate_sf(ucscBuildings_mps, y_add = 0.05), aes(geometry = geometry), 
          fill = "goldenrod4", colour = "goldenrod", alpha = 0.7) +
  #Greenspace
  geom_sf(data = rotate_sf(ucsc, y_add = 0.1), aes(geometry = geometry), fill = "lightgreen", 
          colour = "grey5", alpha = 0.2) +
  geom_sf(data = rotate_sf(ucscGreenspace, y_add = 0.1), aes(geometry = geometry), 
          fill = "lightgreen", colour = "forestgreen", alpha = 0.5) +
  geom_sf(data = rotate_sf(ucscGreenspace_trees, y_add = 0.1), aes(geometry = geometry), 
          colour = "forestgreen", size = 0.05, alpha = 0.6) +
  annotate("richtext", x = -191.42, y = 67.9, label = "<span style='font-size:40px;font-family:Oswald;color:#000000'><b>Greenspace</b></span><br>Parks, Trees,<br>Gardens, Commons",
           colour = "grey10", fill = NA, label.colour = NA, lineheight = 0.4, size = 8,
           family = "Roboto Condensed") +
  annotate("richtext", x = -191.42, y = 67.85, label = "<span style='font-size:40px;font-family:Oswald;color:#000000'><b>Buildings</b></span><br>Offices, Classrooms,<br>Parking Structures",
           colour = "grey10", fill = NA, label.colour = NA, lineheight = 0.4, size = 8,
           family = "Roboto Condensed") +
  annotate("richtext", x = -191.42, y = 67.80, label = "<span style='font-size:40px;font-family:Oswald;color:#000000'><b>Roads</b></span><br>Streets, Paths, Walkways",
           colour = "grey10", fill = NA, label.colour = NA, lineheight = 0.4, size = 8,
           family = "Roboto Condensed") +
  xlim(-191.52, -191.4) +
  labs(title = "University of California, Santa Cruz<br>Campus Features",
       caption = "Source: OpenStreetMaps | #30DayMapChallenge | Day 15 - OpenStreetMaps | @hdailey") +
  theme_void() +
  theme(text = element_text(family = "Roboto Condensed", size = 30),
        plot.title = element_markdown(family = "Oswald", size = 48, face = "bold", colour = "goldenrod", 
                                      lineheight = 0.3, hjust = 0.5),
        plot.title.position = "plot",
        plot.caption = element_text(colour = "grey45", hjust = 0.5, size = 14, margin = margin(t = -5)),
        plot.background = element_rect(fill = "grey95", colour = "grey95"))

# Save ####
ggsave(plot = map3d, path = here::here("2023/maps/"), width = 4, "15_openstreetmap.png", dpi = 320)
