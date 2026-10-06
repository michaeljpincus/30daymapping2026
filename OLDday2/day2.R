library(osmdata)
library(sf)
library(tidyverse)
library(patchwork)
library(png)
library(grid)

### Vassar College

campus <- opq(bbox = "Poughkeepsie, New York", timeout = 60) |>
  add_osm_feature(key = "amenity", value = "college") |>
  osmdata_sf()

campus$osm_polygons |> select(osm_id, name) |> print()

vassar <- bind_rows(
  campus$osm_polygons,
  campus$osm_multipolygons
) |>
  filter(str_detect(name, "Vassar"))

bb <- st_bbox(vassar)

streets <- opq(bbox=bb, timeout=60) |>
  add_osm_feature(key='highway') |>
  osmdata_sf()

lines <- streets$osm_lines

lines_campus <- st_intersection(
  st_make_valid(lines),
  st_make_valid(st_union(vassar))
)

vassar_plot<-ggplot(lines_campus) +
  geom_sf(color='white', linewidth=0.3) +
  theme_void() +
  theme(
    plot.background=element_rect(fill='#641A2B', color=NA)
  )

### University of Pennsylvania

philly_campus <- opq(bbox = "Philadelphia, Pennsylvania", timeout = 60) |>
  add_osm_feature(key = "amenity", value = "university") |>
  osmdata_sf()

penn <- bind_rows(
  philly_campus$osm_polygons,
  philly_campus$osm_multipolygons
) |>
  filter(str_detect(name, "University of Pennsylvania"))

bb_penn <- st_bbox(penn)

penn_streets <- opq(bbox=bb_penn, timeout=60) |>
  add_osm_feature(key='highway') |>
  osmdata_sf()

penn_lines <- penn_streets$osm_lines

penn_lines_campus <- st_intersection(
  st_make_valid(penn_lines),
  st_make_valid(st_union(penn))
)

penn_plot <- ggplot(penn_lines_campus) +
  geom_sf(color='#990000', linewidth=0.3) +
  theme_void() +
  theme(
    plot.background=element_rect(fill='#011F5B', color=NA)
  )

### GWU

gw_campus <- opq(bbox = "Washington, District of Columbia", timeout = 60) |>
  add_osm_feature(key = "amenity", value = "university") |>
  osmdata_sf()

gw <- bind_rows(
  gw_campus$osm_polygons,
  gw_campus$osm_multipolygons
) |>
  filter(str_detect(name, "The George Washington University"))

gw_bb <- st_bbox(gw)

gw_streets <- opq(bbox=gw_bb, timeout=60) |>
  add_osm_feature(key='highway') |>
  osmdata_sf()

gw_lines <- gw_streets$osm_lines

gw_lines_campus <- st_intersection(
  st_make_valid(gw_lines),
  st_make_valid(st_union(gw))
)

gw_plot <- ggplot(gw_lines_campus) +
  geom_sf(color='#D6BF91', linewidth=0.3) +
  theme_void() +
  theme(
    plot.background=element_rect(fill='#033C5A', color=NA)
  )

ggsave("gw_plot.png", gw_plot, width = 6, height = 6, dpi = 300, bg = "#033C5A")
ggsave('vassar_plot.png', vassar_plot, width = 6, height = 6, dpi=300, bg='#641A2B')
ggsave('penn_plot.png', penn_plot, width =6, height=6, dpi=300, bg='#011F5B')


### Making the final plot

img_panel <- function(path, caption) {
  img <- readPNG(path)
  ggplot() +
    annotation_custom(rasterGrob(img, interpolate=TRUE)) +
    labs(caption=caption) +
    base_theme +
    theme(plot.caption = element_text(hjust = 0.5, size = 12, margin = margin(t = 8)))
}

p1 <- img_panel('vassar_plot.png', 'Vassar College')
p2 <- img_panel('gw_plot.png', 'The George Washington University')
p3 <- img_panel('penn_plot.png', 'University of Pennsylvania')

(p1 | p2 | p3) +
  plot_annotation(
    title = 'Day 2: Lines',
    subtitle = '#30DayMapChallenge',
    caption = 'Map by Michael Pincus | Data from OpenStreetMap',
    theme = base_theme
  )
