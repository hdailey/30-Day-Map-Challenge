library(tidyverse)
library(magick)

file.remove(here::here("2023/maps/00_finalimg.png"))

img_list <- list.files(path = here::here("2023/maps/"), full.names = TRUE)

img_montage <- image_read(img_list) %>% 
  image_montage(tile = '3x4', 
                geometry = "x2000+0+0", 
                shadow = FALSE, 
                bg = "grey95")

image_write(img_montage, format = "png", 
            path = here::here("2023/maps/00_finalimg.png"), 
            quality = 100)
