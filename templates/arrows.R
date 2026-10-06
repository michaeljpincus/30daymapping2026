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

# Base Theme

font_add_google("Google Sans", "google")
showtext_auto()

update_geom_defaults("text", list(family = "google"))

base_theme <- theme_void(base_family = "google") +
  theme(
    plot.title    = element_text(size = 20, face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0, color = "gray40", margin = margin(b = 15)),
    plot.caption  = element_text(size = 9, hjust = 0, color = "gray50", margin = margin(t = 15)),
    legend.position = c(0, 0.1),
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    legend.title.position = "top",
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )


states <- ne_download(
  scale = 10,
  type = "admin_1_states_provinces_lakes",
  returnclass = "sf"
)

cities <- ne_download(
  scale=10,
  type='populated_places'
)

ny <- states |>
  filter(name=='New York')

ny <- st_transform(ny, 2272)

cities <- st_transform(cities, 2272)

ny_cities <- st_filter(cities, ny, predicate=st_within)

top5_cities <- ny_cities |> slice_max(POP_MAX, n=5)
gap_label <- 60000   # feet to leave between label center and arrow start
gap_city  <- 30000   # feet to stop short of the city center

top5_xy <- top5_cities |>
  mutate(x  = st_coordinates(geometry)[, 1],
         y  = st_coordinates(geometry)[, 2],
         lx = x + c(250000, -250000, 250000, -250000, 250000),
         ly = y + c(-150000, 150000, 150000, -150000, 100000)) |>
  st_drop_geometry() |>
  mutate(dx = x - lx, dy = y - ly,
         len = sqrt(dx^2 + dy^2),
         ux = dx / len, uy = dy / len,           # unit vector from label to city
         sx = lx + ux * gap_label,               # arrow start, off the label
         sy = ly + uy * gap_label,
         ex = x - ux * gap_city,                 # arrow end, off the circle
         ey = y - uy * gap_city)

ggplot() +
  geom_sf(data = ny, color = NA) +
  geom_sf(data = ny_cities, aes(size = POP_MAX), color = 'red', alpha = 0.5) +
  scale_size_continuous(
    range = c(1, 20),
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  geom_curve(data = top5_xy,
             aes(x = sx, y = sy, xend = ex, yend = ey),
             curvature = 0.3, angle = 90, ncp = 10,
             arrow = arrow(length = unit(0.01, "npc"), type = 'closed'),
             linewidth = 0.5) +
  geom_shadowtext(
    data = top5_xy,
    aes(x = lx, y = ly, label = NAME),
    family = 'google',
    fontface = "bold",
    color = "black",      # text color
    bg.color = "white",   # halo color
    bg.r = 0.25           # halo radius, bigger = thicker halo
  ) +
  labs(title = 'New York State',
       subtitle = 'Largest Cities',
       caption = 'Data from Natural Earth',
       size = 'Population') +
  base_theme

