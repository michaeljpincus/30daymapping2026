library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(shadowtext)
library(sysfonts)
library(showtext)

font_add_google("Google Sans", "google")
showtext_auto()
showtext_opts(dpi = 200)

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

# ---- 1. Inputs ---------------------------------------------------------------

states <- st_read('states.geojson')
amtrak <- st_read('amtrak.geojson')

# amtrak:  sf lines, one row per line/route
# Adjust these to match your column names:
state_col <- 'name'    # state name column in `states`
line_col  <- 'name'       # route/line name column in `amtrak`


# ---- 2. Prep geometries -------------------------------------------------------
# Use an equal-area CRS so every state is drawn consistently
crs_use <- 5070  # NAD83 / Conus Albers

states <- states |>
  st_make_valid() |>
  st_transform(crs_use) |>
  rename(state = all_of(state_col)) |>
  select(state, geometry)

amtrak <- amtrak |>
  st_make_valid() |>
  st_transform(crs_use) |>
  rename(line = all_of(line_col)) |>
  select(line, geometry)

# ---- 3. Crop lines to each state's boundary ------------------------------------
# st_intersection returns each line piece with the attributes of the state it falls in
amtrak_clipped <- suppressWarnings(st_intersection(amtrak, states)) |>
  st_collection_extract("LINESTRING") |>   # drop stray points from touching boundaries
  st_cast("MULTILINESTRING")

# ---- 5. One panel per state ----------------------------------------------------
make_panel <- function(st) {
  poly  <- filter(states, state == st)
  lines <- filter(amtrak_clipped, state == st)

  bb   <- st_bbox(poly)
  cx   <- mean(c(bb["xmin"], bb["xmax"]))
  cy   <- mean(c(bb["ymin"], bb["ymax"]))
  half <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"]) / 2 * 1.05

  ggplot() +
    geom_sf(data = poly, fill = "white", color = NA) +
    geom_sf(data = lines, color = "#1a1a2e", linewidth = 0.5, show.legend = FALSE) +
    coord_sf(xlim = c(cx - half, cx + half),
             ylim = c(cy - half, cy + half),
             expand = FALSE) +
    labs(title = st) +
    base_theme +
    theme(
      plot.background = element_rect(fill = NA, color = NA),   # transparent, so the overall color shows
      plot.title = element_text(family = "google", size = 16, face = "bold",
                                hjust = 0.5, color = "white")
    )
}

panels <- lapply(sort(states$state), make_panel)

big_plot <- wrap_plots(panels, ncol = 8) +
  plot_annotation(
    title   = "Amtrak Lines by State",
    caption = "#30DayMapChallenge Day 2: Lines | by Michael Pincus",
    theme = theme(
      plot.background = element_rect(fill = "#1a1a2e", color = NA),
      plot.title   = element_text(family = "google", size = 40, face = "bold",
                                  hjust = 0.5, color = "white"),
      plot.caption = element_text(family = "google", size = 14, color = "grey70")
    )
  ) &
  theme(plot.margin = margin(15, 15, 15, 15))

ggsave("amtrak_by_state.png", big_plot, width = 24, height = 18, dpi = 200, bg = "#1a1a2e")
