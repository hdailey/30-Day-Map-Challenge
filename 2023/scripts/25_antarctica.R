# 30 Day Map Challenge, Day 25 - Antarctica ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)
library(sf)
library(showtext)

# Fonts
font_add_google("Georama")
showtext_auto()

# Data
ata <- st_read(here::here("2023/data/25_antarctica/ATA_adm0.shp")) %>%
  st_transform("+proj=laea +lat_0=-90 +lon_0=0")

ataMIZ <- st_read(here::here("2023/data/25_antarctica/nic_miz2023340sc_pl_a.shp")) %>%
  st_transform("+proj=laea +lat_0=-90 +lon_0=0") %>%
  mutate(ICECODE = case_when(ICECODE == "CT18" ~ "1-8",
                             ICECODE == "CT81" ~ "8-10"))
# Map ####
map <- ggplot() +
  geom_sf(data = ata, aes(geometry = geometry), fill = "grey95") +
  geom_sf(data = ataMIZ, aes(geometry = geometry, fill = ICECODE)) +
  geom_sf(data = ataElev, aes(geometry = geometry)) +
  scale_fill_manual(values = c("yellow", "red")) +
  labs(title = "Antarctic Marginal Ice Zone (MIZ)",
       subtitle = "6 December 2023",
       caption = "Source: US National Ice Center & UC Berkeley | #30DayMapChallenge | Day 25: Antarctica | @hdailey") +
  guides(fill = guide_legend(title = "Sea Ice Concentration (Tenths)")) +
  cowplot::theme_map() +
  theme(text = element_text(family = "Georama", size = 16),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 32),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        plot.caption = element_text(hjust = 0.5, margin = margin(b = 1), size = 12),
        legend.position = c(0,0.085),
        legend.title.align = 0.5,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.box.margin = margin(1, 1, 1, 1),
        legend.box.background = element_rect(fill = "grey98", colour = "grey5"),
        plot.background = element_rect(fill = "lightblue", colour = "lightblue"))

# Save ####
ggsave(plot = map, path = here::here("2023/maps/"), "25_antarctica.png", height = 5, width = 4, units = "in", dpi = 320)
