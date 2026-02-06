library(dplyr)
library(terra)
library(sf)
library(units)
library(igraph)
library(ggplot2)
library(patchwork)
library(cols4all)

setwd("~/Documents/code/dosull.github.io/posts/_2025-11-xx-city-to-sea")

dem <- rast("c2s.tif")
names(dem) <- "Elevation"


linestring_as_matrix <- function(L) {
  (L |> st_coordinates())[, 1:2]
}

join_segments <- function(s1, s2) {
  junction <- st_intersection(s1, s2)
  along_1 <- st_line_project(s1, junction, normalized = TRUE)
  along_2 <- st_line_project(s2, junction, normalized = TRUE)
  m1 <- linestring_as_matrix(s1)
  m2 <- linestring_as_matrix(s2)
  if (along_1 == 0) {
    m1 <- apply(m1, 2, rev)
  }
  if (along_2 == 0) {
    m2 <- m2[-1, ]
  } else {
    m2 <- apply(m2, 2, rev)[-1, ]
  }
  st_linestring(rbind(m1, m2)) |> 
    st_sfc() |>
    st_set_crs(st_crs(s1))
}

switch_xy <- function(df) {
  df |>
    mutate(z = -y, y = x, x = z) |>
    select(-z)
}


path <- st_read("city-to-sea-all.gpkg") |>
  st_transform(2193)

steps <- 0:3000 / 3000

segments <- path |>
  st_cast("LINESTRING")

connections <- segments |>
  st_touches()

start_finish <- which((connections |> lengths()) == 1)
 
sequence <- (segments |> 
  st_touches(sparse = FALSE) |>
  graph_from_adjacency_matrix(mode = "undirected") |>
  shortest_paths(from = start_finish[1], to = start_finish[2]))$vpath |>
  unlist()

path_linestring <- segments$geom[1]
for (i in sequence[-1]) {
  path_linestring <- join_segments(path_linestring, segments$geom[i])
}

points <- path_linestring |>
  st_line_interpolate(steps, normalized = TRUE)

profile <- terra::extract(dem, points |> as("SpatVector"), method = "bilinear") |>
  mutate(Distance = (path_linestring |> st_length()) * steps) |>
  mutate(Distance = drop_units(Distance))

g_profile <- ggplot() +
  geom_area(data = profile, aes(x = Distance, y = Elevation), fill = "grey") +
  coord_fixed(ratio = 8) +
  theme_minimal()

g_profile


named_segments <- st_read("city-to-sea-segments.gpkg") |>
  st_transform(2193)

named_distances <- list() 
for (line in named_segments$geom) { 
  named_distances[[length(named_distances) + 1]] <- 
    st_line_interpolate(line |> st_sfc(), 0.5, normalized = TRUE) |>
    st_set_crs(2193) |>
    st_line_project(line = path_linestring)
}
named_distances <- named_distances |> unlist()

label_pts <- path_linestring |>
  st_line_interpolate(named_distances) |>
  data.frame() |>
  st_sf() |>
  mutate(Distance = named_distances,
         name = named_segments$name) |>
  as("SpatVector") |>
  terra::extract(x = dem, method = "bilinear", bind = TRUE) |>
  st_as_sf() |>
  filter(!is.na(name) & name != "City to Sea Walkway") |>
  group_by(name) |>
  filter(row_number() == 1) 

label_xys <- label_pts |> 
  st_coordinates() |>
  data.frame() |> 
  rename(x = X, y = Y) |>
  switch_xy() |>
  bind_cols(label_pts |> st_drop_geometry())
  

g_profile_labelled <- g_profile + 
  geom_text(
    data = label_xys, 
    aes(x = Distance, y = Elevation, label = name),
    angle = 90, size = 2.5, hjust = 0, vjust = 0.5, nudge_y = 5, 
    check_overlap = TRUE) +
  scale_y_continuous(breaks = seq(0, 150, 50)) +
  coord_fixed(ratio = 8, ylim = c(0, 350)) +
  theme(axis.title.y = element_text(hjust = 0.15))
  
g_profile_labelled

dem_x5 <- dem |> aggregate(5)
slope <- dem_x5 |> terrain(unit = "radians")
aspect <- dem_x5 |> terrain(v = "aspect", unit = "radians")
hillshade <- shade(slope, aspect, direction = 225) |>
  as.data.frame(xy = TRUE) |>
  switch_xy()

path_df <- path_linestring |> 
  st_coordinates() |> 
  data.frame() |>
  select(X, Y) |> 
  rename(x = X, y = Y) |>
  switch_xy()

xmin <- min(hillshade$x)
ymax <- max(hillshade$y)

g_map <- ggplot() +
  geom_raster(
    data = hillshade, 
    aes(x = x, y = y, fill = hillshade), alpha = 0.65) +
  scale_fill_continuous_c4a_seq(palette = "brewer.greens") +
  guides(fill = "none") +
  geom_path(data = path_df, aes(x = x, y = y), 
            linetype = "dashed") +
  geom_text(
    data = label_xys, aes(x = x, y = y, label = name),
    angle = 90, size = 2.5, 
    hjust = c(rep(0:1, 15), 0), vjust = 0.5, 
    nudge_y = c(rep(c(25, -25), 15), 25),
    check_overlap = TRUE) +
  annotate("segment",
    x = xmin + 250, y = ymax + 100,
    xend = xmin + 150, yend = ymax + 100,
    arrow = arrow(type = "closed", 
                  length = unit(0.1, "npc"), angle = 20)) +
  annotate("text",
    x = xmin + 80, y = ymax + 100, label = "N", size = 5) +
  coord_equal(ylim = range(hillshade$y) + c(-200, 200)) +
  theme_void()

(g_map / g_profile_labelled) + 
  plot_layout(heights = c(2, 1))

