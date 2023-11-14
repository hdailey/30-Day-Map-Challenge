# 30 Day Map Challenge, Day 14 - Europe ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)
library(showtext)
library(eurostat)

# Fonts
font_add_google("Smooch Sans", db_cache = FALSE)
showtext_auto()

# Data
euCountries <- eu_countries

europe <- rnaturalearthdata::countries110 %>%
  sf::st_as_sf() %>%
  filter(name %in% c(euCountries$name))

europeRenewable <- read_csv(here::here("2023/data/14_europe.csv")) %>%
  filter(Entity %in% c(euCountries$name)) %>%
  left_join(europe, by = c("Entity" = "name"))

# Map ####
plotEU <- ggplot(europeRenewable) +
  geom_sf(aes(geometry = geometry, fill = (`Renewables (% equivalent primary energy)`/100)),
          colour = "#000000", linewidth = 0.1) +
  annotate("text", x = -33.031, y = 30.496, label = "Percent Share of Primary Energy\nSourced from Renewables",
           family = "Smooch Sans", size = 16, lineheight = 0.3) +
  scale_fill_distiller(palette = "Greens", direction = 1, labels = scales::label_percent()) +
  labs(caption = "Source: OurWorldinData.org | #30DayMapChallenge | Day 14: Europe | @hdailey") +
  theme_void() +
  theme(text = element_text(family = "Smooch Sans", size = 28),
        legend.direction = "horizontal",
        legend.position = c(0.25,0.3),
        legend.text = element_text(margin = margin(t = -10)),
        plot.caption = element_text(colour = "grey55", size = 18, hjust = 0.5,
                                    margin = margin(b = 5)),
        plot.background = element_rect(fill = "grey95", colour = "grey95")) +
  guides(fill = guide_colourbar(title = "", frame.colour = "#000000", ticks.colour = "#000000",
                                frame.linewidth = 0.25, ticks.linewidth = 0.25, barwidth = 8))

# Save ####
ggsave(plot = plotEU, path = here::here("2023/maps/"), "14_europe.png", dpi = 320)
