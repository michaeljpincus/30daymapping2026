library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(scales)
library(shadowtext)
library(sysfonts)
library(showtext)

options(scipen=999)

font_add_google("Google Sans", "google")
showtext_auto()

countries <- ne_download(
  scale = 50, 
  type = 'admin_0_countries_lakes',
  returnclass = 'sf'
)

cities <- ne_download(
  scale=110,
  type='populated_places'
)

megacities <- cities |> 
  filter(NAME %in% c('Mexico City', 'Los Angeles', 'São Paulo', 'Buenos Aires', 'Rio de Janeiro', 'Karachi', 'New York', 'New Delhi', 'Kolkata', 'Mumbai', 'Dhakha', 'Beijing', 'Tokyo', 'Shanghai', 'Manila'))

ggplot() +
  geom_sf(data = countries, fill='azure2', color = "white") +
  geom_sf(data = megacities, size=0.9, color='azure4') + 
  geom_text_repel(
    data = megacities,
    aes(label = NAME, geometry = geometry),
    stat = "sf_coordinates",
    family = 'google',
    size = 3,
    #bg.color='white',
    #bg.r=0.15,
    min.segment.length = 3,
    box.padding = 0.2,
    point.padding = 0,
    fontface = 'bold',
    force = 3,
    max.overlaps = Inf,
    segment.color = "grey30"
  ) + 
  labs(title = "World Megacities",
       caption = "Data from Natural Earth",
       subtitle = 'Megacity: a very large metropolitan area or urban agglomeration with a total population of 10 million or more people') +
  coord_sf(crs = "+proj=wintri", datum=NA) +
  base_theme
