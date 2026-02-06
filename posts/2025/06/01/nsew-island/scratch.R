library(sf)
library(units)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

setwd("~/Documents/code/dosull.github.io/posts/2025-06-01-nsew-island")

bbox_split <- function(bb, c, half = "N") {
  key <- list(N = "ymin", S = "ymax", E = "xmin", W = "xmax")
  coord <- list(N = "Y", S = "Y", E = "X", W = "X")
  bb |> replace(key[[half]], c[, coord[[half]]]) |>
    st_as_sfc() |>
    as.data.frame() |>
    st_sf()
}

nz <- st_read("nz-islands.gpkg")
bb <- nz |> st_bbox()
centroid <- nz |> 
  st_union() |> 
  st_centroid() |> 
  st_coordinates()

ns_split <- bbox_split(bb, centroid, "N") |>
  mutate(north_c = TRUE) |>
  bind_rows(bbox_split(bb, centroid, "S") |>
    mutate(north_c = FALSE))

ew_split <- bbox_split(bb, centroid, "E") |>
  mutate(east_c = TRUE) |>
  bind_rows(bbox_split(bb, centroid, "W") |>
    mutate(east_c = FALSE))

nz_split <- nz |> 
  st_intersection(ns_split) |>
  st_intersection(ew_split) |>
  mutate(ns_correct = north_c == north,
         ew_correct = east_c == north) |>
  group_by(ns_correct, ew_correct) |>
  summarise() |>
  mutate(area = st_area(geom) |> set_units("km^2"))

nz_split |> st_drop_geometry()

map1 <- ggplot(nz_split) +
  geom_sf(aes(fill = ns_correct), lwd = 0) +
  scale_fill_manual(breaks = as.logical(0:1),
                    values = c("red", "lightgrey")) +
  geom_hline(aes(yintercept = centroid[2]), linetype = "dotted") +
  guides(fill = "none") +
  ggtitle('North-South wrong') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

map2 <- ggplot(nz_split) +
  geom_sf(aes(fill = ew_correct), lwd = 0) +
  scale_fill_manual(breaks = as.logical(0:1),
                    values = c("red", "lightgrey")) +
  geom_vline(aes(xintercept = centroid[1]), linetype = "dotted") +
  guides(fill = "none") +
  ggtitle('East-West wrong') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

map1 + map2

# ggplot(nz) +
#   geom_sf(fill = "lightgrey", lwd = 0) +
#   geom_abline(aes(slope = -2, intercept = 8.88e6), linetype = "dotted") +
#   theme_void()

hulls <- nz |>
  group_by(north) |>
  summarise() |>
  st_convex_hull()

g1 <- ggplot(nz) +
  geom_sf(lwd = 0) +
  geom_sf(data = hulls, fill = NA, colour = "red") +
  geom_abline(aes(intercept = 9.6125e6, slope = -2.414214), 
              linetype = "dashed") +
  annotate("polygon", x = c(1.65e6, 1.65e6, 1.75e6, 1.75e6),
           y = c(5.4e6, 5.5e6, 5.5e6, 5.4e6), fill = NA, colour = "black") +
  theme_void()

g2 <- ggplot(nz) +
  geom_sf() +
  geom_sf(data = hulls, fill = NA, colour = "red") +
  coord_sf(xlim = c(1.65e6, 1.75e6), ylim = c(5.4e6, 5.5e6), 
           expand = FALSE, datum = 2193) +
  geom_abline(aes(intercept = 9.6125e6, slope = -2.414214), 
              linetype = "dashed") +
  theme_void() +
  theme(panel.border = element_rect(fill = NA))

g1 + g2

bb <- nz |> 
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(1e4, nQuadSegs = 0)

voronoi_islands <- nz |>
  ms_simplify() |>
  st_cast("POINT") |> 
  st_union() |> 
  st_voronoi() |> 
  st_cast() |> 
  st_as_sf() |> 
  st_intersection(bb) |>
  st_join(nz) |>
  group_by(north) |>
  summarise()

g1 <- ggplot(voronoi_islands) + 
  geom_sf(aes(fill = north), lwd = 0) + 
  scale_fill_brewer(palette = "Paired") +
  geom_sf(data = nz, lwd = 0) +
  guides(fill = "none") +
  theme_void()

g2 <- ggplot(voronoi_islands) + 
  geom_sf(aes(fill = north), lwd = 0) + 
  scale_fill_brewer(palette = "Paired") +
  geom_sf(data = nz) +
  guides(fill = "none") +
  coord_sf(xlim = c(1.65e6, 1.75e6), ylim = c(5.4e6, 5.5e6), 
           expand = FALSE, datum = 2193) +
  theme_void()

g1 + g2
