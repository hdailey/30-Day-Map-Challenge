# 30 Day Map Challenge, Day 13 - Chloropleth ####

# Libraries, Fonts, Data ####
# Libraries
library(tidyverse)
library(showtext)
library(patchwork)

# Fonts
font_add_google("")
font_add_google("Merriweather")
showtext_auto()

# Data
warPeace <- read_csv(here::here("2023/data/13_chloropleth.csv"))
world <- rnaturalearthdata::countries110 %>%
  sf::st_as_sf()

warPeace2020 <- warPeace %>%
  filter(Year == 2020) %>%
  full_join(world, by = c("Code" = "gu_a3")) %>%
  filter(Code != "ATA")

warPeace2000 <- warPeace %>%
  filter(Year == 2000) %>%
  full_join(world, by = c("Code" = "gu_a3")) %>%
  filter(Code != "ATA")

warPeace <- warPeace2020 %>%
  rbind(warPeace2000) %>%
  filter(Year %in% c(2000, 2020))
  
# Map ####
plotChloropleth <- ggplot(warPeace) +
  geom_sf(aes(geometry = geometry, fill = military_expenditure_share_gdp), colour = "#000000",
          linewidth = 0.05) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_void() +
  facet_wrap(~Year, nrow = 2) +
  labs(title = "Military Expenditure as a Share of GDP",
       caption = "Source: OurWorldinData.org | #30DayMapChallenge | Day 13: Chloropleth | @hdailey") +
  theme(text = element_text(family = "Merriweather", size = 32),
        plot.title = element_text(face ="bold", hjust = 0.5, size = 48),
        plot.title.position = "plot",
        plot.caption = element_text(colour = "grey55", hjust = 0.5, size = 20),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        legend.justification = "center",
        legend.position = "top",
        legend.direction = "horizontal",
        legend.text = element_text(size = 24, margin = margin(t = -12)),
        legend.title = element_text(size = 24)) +
  guides(fill = guide_colorbar(title = "Percent GDP", title.vjust = 0.75,
                               barwidth = 10, barheight = 0.75))
  
# Save ####
ggsave(plot = plotChloropleth, path = here::here("2023/maps/"), "13_chloropleth.png", dpi = 320)
