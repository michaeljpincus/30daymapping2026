library(geodata)
library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(ggrepel)


pak <- gadm(country = "PAK", level = 0, path = tempdir())
dem <- elevation_30s(country = "PAK", path = tempdir())

dem <- crop(dem, pak)
dem <- mask(dem, pak)
names(dem) <- 'elevation'

slope  <- terrain(dem, "slope",  unit = "radians")
aspect <- terrain(dem, "aspect", unit = "radians")
hill   <- shade(slope, aspect, angle = 40, direction = 270)

cities <- data.frame(
  name = c("Islamabad", "Lahore", "Karachi", "Peshawar", "Quetta",
           "Multan", "Faisalabad", "Hyderabad", "Gilgit", "Skardu",
           "Gwadar", "Sukkur", "Muzaffarabad"),
  lon  = c(73.05, 74.35, 67.01, 71.58, 66.99,
           71.52, 73.08, 68.37, 74.31, 75.56,
           62.32, 68.86, 73.47),
  lat  = c(33.69, 31.52, 24.86, 34.01, 30.18,
           30.20, 31.42, 25.39, 35.92, 35.30,
           25.12, 27.70, 34.37)
)

cities_sf <- st_as_sf(cities, coords=c('lon', 'lat'), crs=4326)

ggplot() +
  geom_spatraster(data = dem) +
  scale_fill_hypso_tint_c(
    palette = "wiki-2.0_hypso",   # hypsometric (elevation) colors
    na.value = "transparent",
    name = "Elevation (m)"
  ) +
  geom_sf(data = cities_sf, color = "black", size = 1.8) +
  geom_text_repel(
    data = cities,
    aes(x = lon, y = lat, label = name),
    size = 3, fontface = "bold", family='google',
    bg.color = "white", bg.r = 0.15,   # white halo for readability
    min.segment.length = 1
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "Digital Elevation Model of Pakistan",
       x = NULL, y = NULL,
       caption = "Data: SRTM via geodata | GADM")  + 
  base_theme + 
  theme(legend.position = "right")

### Doing this in rayshader 

library(rayshader)
library(terra)
library(sf)

# --- Convert DEM raster to a matrix ---
# (dem should already be cropped/masked from before)
elmat <- raster_to_matrix(dem)   # rayshader can take a SpatRaster directly in recent versions

# Downsample so it renders quickly (Pakistan at 1 km is large)
elmat_small <- resize_matrix(elmat, 0.5)   # try 0.25 if slow

# --- Build the 3D scene ---
# Pick a colour palette (these are the wiki-style hypsometric tints)
pal <- tidyterra::hypso.colors2(256, palette = "wiki-2.0_hypso")

elmat_small |>
  height_shade(texture = pal) |>
  add_shadow(ray_shade(elmat_small, zscale = 200, multicore = TRUE), 0.5) |>
  add_shadow(ambient_shade(elmat_small, zscale = 200), 0) |>
  plot_3d(elmat_small,
          zscale = 200,            # larger = flatter; smaller = more exaggerated
          fov = 0, theta = 20, phi = 40,
          zoom = 0.6,
          windowsize = c(1200, 900),
          solid = TRUE,
          background = "white")

render_snapshot("pakistan_3d.png")