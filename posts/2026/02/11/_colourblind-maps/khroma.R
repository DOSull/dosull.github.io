library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(colorblindr)

setwd(dirname(rstudioapi::documentPath()))

gdf <- st_read("sf_clustering_data.gpkg") |>
  select(-(1:4), -16, -27) |>
  drop_na()

xr <- c(1.82e6, 1.87e6)
yr <- c(6.3e5, 6.6e5)

bb <- st_bbox(c(xmin = xr[1], ymin = yr[1], xmax = xr[2], ymax = yr[2])) |>
  st_as_sfc() |>
  data.frame() |>
  st_sf(crs = st_crs(df))

land <- df |>
  group_by(1) |>
  summarise() |>
  st_intersection(bb)

df <- gdf |> st_drop_geometry()
d <- dist(df)

clusters <- 5:15 |>
  lapply(kmeans, x = d) |>
  lapply("[", "cluster") |>
  data.frame()

names(clusters) <- paste("k", c(rep("0", 5), rep("", 6)), 5:15, sep = "")
clusters <- clusters |>
  mutate(across(everything(), ~ as.factor(.), .names = "{col}d"))

gdf <- bind_cols(gdf, clusters)

map <- ggplot() + 
  geom_sf(data = land, fill = NA, colour = "black") +
  geom_sf(data = gdf, aes(fill = k05d), colour = "lightgrey") +
  coord_sf(xlim = xr, ylim = yr, expand = FALSE) +
  theme_void() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    panel.background = element_rect(fill = "white", colour = NA))
map

cvd_grid(map)

map <- ggplot() + 
  geom_sf(data = land, fill = NA, colour = "black") +
  geom_sf(data = gdf, aes(fill = k05d), colour = "lightgrey") +
  # scale_fill_brewer(palette = "Dark2") +
  scale_fill_mediumcontrast() +
  coord_sf(xlim = xr, ylim = yr, expand = FALSE) +
  theme_void() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    panel.background = element_rect(fill = "white", colour = NA))
map

cvd_grid(map)
