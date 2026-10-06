library(sf)
library(readr)
library(rnaturalearth)
library(ggrepel)

cities <- read_csv("taste_cities.csv")

cities_sf <- st_as_sf(
  cities,
  coords = c("long", "lat"),  # x (lon) first, then y (lat)
  crs = 4326                            # WGS84
)

taste_cities <- st_as_sf(
  cities,
  coords = c("long", "lat"),  # x (lon) first, then y (lat)
  crs = 4326                            # WGS84
)

taste_cities$name[taste_cities$name == "Hatay (Antakya)"] <- "Hatay"
taste_cities$name[taste_cities$name == "Overstrand Hermanus"] <- "Hermanus"
taste_cities$name[taste_cities$name == "Macao"] <- "Macau"


countries <- ne_download(
  scale = 50,
  type = 'admin_0_countries_lakes',
  returnclass = 'sf'
)

ggplot() +
  geom_sf(data = countries, fill='azure2', color = "white") +
  geom_sf(data = taste_cities, size=0.9, color='azure4') +
  geom_text_repel(
    data = taste_cities,
    aes(label = name, geometry = geometry),
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
  labs(title = "UNESCO Cities of Gastronomy",
       caption = "Data from Natural Earth",
       subtitle = 'Cities submit bids to UNESCO to be designated, with a review every four years') +
  coord_sf(crs = "+proj=wintri", datum=NA) +
  base_theme
