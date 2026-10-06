# ============================================================================
# Day 20: Water - Edmund Fitzgerald Tribute
# Lake Superior Bathymetry
# #30DayMapChallenge 2025
# ============================================================================

library(tidyverse)
library(sf)
library(terra)
library(marmap)
library(ggspatial)
library(overturemapsr)
# ============================================================================
# COLOR PALETTE - Moody blues with signature pink accent
# ============================================================================

# Dark moody bathymetry palette (deep to shallow)
bathy_palette <- c(
  "#0a1628",
  "#0d2137",
  "#102a45",
  "#153654",
  "#1a4263",
  "#1f4e72",
  "#245a81",
  "#2a6690"
)

# Signature pink for accents
accent_pink <- "#D93B54"
light_pink <- "#FDE4E8"

# Background
bg_dark <- "#050a12"

# ============================================================================
# Lake Superior Coastline
# ============================================================================

library(rnaturalearth)

# Get lakes - includes Great Lakes
lakes <- ne_download(
  scale = 10,
  type = "lakes",
  category = "physical",
  returnclass = "sf"
)

# Filter to Lake Superior
lake_superior <- lakes %>%
  filter(name == "Lake Superior")
# ============================================================================
# GET BATHYMETRY DATA
# ============================================================================

library(marmap)
# Lake Superior bounding box
lake_superior_bbox <- c(
  xmin = -92.2,
  xmax = -84.3,
  ymin = 46.4,
  ymax = 49.0
)

# Get bathymetry from NOAA using marmap
superior_bathy <- getNOAA.bathy(
  lon1 = lake_superior_bbox["xmin"],
  lon2 = lake_superior_bbox["xmax"],
  lat1 = lake_superior_bbox["ymin"],
  lat2 = lake_superior_bbox["ymax"],
  resolution = 1,
  keep = TRUE
)

# Convert to data frame for ggplot
bathy_df <- fortify.bathy(superior_bathy)

###data points to map

# Route - starts on land at Superior, WI
route_points <- data.frame(
  lon = c(-91.8, -90.8, -90.0, -89.0, -88.0, -87.0, -86.0, -85.1),
  lat = c(46.7, 47.2, 47.5, 47.8, 48.0, 47.7, 47.2, 46.998)
)

intended_route_points <- data.frame(
  lon = c(-85.1, -84.75, -84.5),
  lat = c(46.998, 46.75, 46.5)
)

route_line <- route_points %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

intended_route_line <- intended_route_points %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

key_locations <- data.frame(
  lon = c(-91.8, -84.75),
  lat = c(46.7, 46.75),
  label = c("Superior, WI", "Whitefish Point"),
  hjust = c(1.1, -0.1)
)

# Wreck site
wreck_site <- data.frame(
  lon = -85.1,
  lat = 46.998,
  label = "Wreck Site\n530 ft depth"
)

# Destination
destination_label <- data.frame(
  lon = -84.45,
  lat = 46.35,
  label = "To Soo Locks\n& Detroit"
)

# Context labels
context_labels <- data.frame(
  lon = c(-88.0, -88.5, -84.5, -92.25),
  lat = c(49.3, 46.15, 46.1, 46.20),
  label = c("CANADA", "U.S.A.", "MICHIGAN", "MINNESOTA"),
  size = c(7, 7, 3.5, 3.5)
)



# ============================================================================
# PLOT
# ============================================================================

ggplot() +
  # Bathymetry
  geom_contour_filled(
    data = bathy_df,
    aes(x = x, y = y, z = z),
    breaks = c(-Inf, -400, -300, -200, -150, -100, -50, -20, 0)
  ) +
  
  scale_fill_manual(
    values = bathy_palette,
    name = "Depth (m)",
    labels = c(">400", "300-400", "200-300", "150-200", 
               "100-150", "50-100", "20-50", "0-20")
  ) +
  
  # Lake outline
  geom_sf(
    data = lake_superior,
    fill = NA,
    color = light_pink,
    linewidth = 0.5
  ) +
  
  # Context labels
  geom_text(
    data = context_labels,
    aes(x = lon, y = lat, label = label, size = size),
    color = accent_pink,
    family = "sans",
    fontface = "bold"
  ) +
  scale_size_identity() +
  
  # Intended route (dashed)
  geom_sf(
    data = intended_route_line,
    color = light_pink,
    linewidth = 0.8,
    linetype = "dashed",
    alpha = 0.5
  ) +
  
  # Actual ship's route
  geom_sf(
    data = route_line,
    color = accent_pink,
    linewidth = 1.2
  ) +
  
  # Wreck site marker
  geom_point(
    data = wreck_site,
    aes(x = lon, y = lat),
    color = accent_pink,
    size = 4,
    shape = 4,
    stroke = 2
  ) +
  
  # Wreck site label
  geom_text(
    data = wreck_site,
    aes(x = lon, y = lat, label = label),
    color = light_pink,
    size = 3,
    nudge_y = 0.30,
    nudge_x = -0.2,
    family = "serif",
    fontface = "italic",
    hjust = 1
  ) +
  
  # Key location points
  geom_point(
    data = key_locations,
    aes(x = lon, y = lat),
    color = light_pink,
    size = 2
  ) +
  
  # Key location labels
  geom_text(
    data = key_locations,
    aes(x = lon, y = lat, label = label, hjust = hjust),
    color = light_pink,
    size = 2.5,
    family = "sans",
    nudge_y = -0.08
  ) +
  
  # Destination label
  geom_text(
    data = destination_label,
    aes(x = lon, y = lat, label = label),
    color = light_pink,
    size = 2.5,
    family = "sans",
    lineheight = 0.9,
    hjust = 0
  ) +
  
  coord_sf(
    xlim = c(-92.8, -84.0),
    ylim = c(45.5, 49.5),
    expand = FALSE
  ) +
  
  labs(
    title = "LAKE SUPERIOR",
    subtitle = "Bathymetry & the Final Voyage of the SS Edmund Fitzgerald",
    caption = "November 10, 1975 (The Gales of November!) | #30DayMapChallenge Day 20: Water\nData: NOAA, Natural Earth | Author: Elizabeth Delmelle"
  ) +
  
  # Legend as horizontal, bottom left
  guides(
    fill = guide_legend(
      nrow = 1,
      title.position = "top",
      label.position = "bottom"
    )
  ) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_dark, color = NA),
    panel.background = element_rect(fill = bg_dark, color = NA),
    plot.title = element_text(
      color = light_pink,
      size = 24,
      face = "bold",
      hjust = 0,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      color = light_pink,
      size = 12,
      face = "italic",
      hjust = 0,
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      color = "#666666",
      size = 8,
      hjust = 0,
      margin = margin(t = 15)
    ),
    legend.position = c(0.10, 0.08),
    legend.direction = "horizontal",
    legend.title = element_text(color = light_pink, size = 9),
    legend.text = element_text(color = light_pink, size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.6, "cm"),
    legend.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

