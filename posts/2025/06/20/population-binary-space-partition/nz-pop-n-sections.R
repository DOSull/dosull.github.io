library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
library(spatstat)   # for weighted median function
library(rlist)

setwd("~/Documents/code/dosull.github.io/posts/_2025-06-20-population-quadtree")
pop_grid <- read.csv("nz-pop-grid-250m.csv") |>
  dplyr::filter(x < 2.2e6)
nz <- st_read("nz-small.gpkg")

get_rectangle <- function(x1, x2, y1, y2) {
  st_polygon(list(matrix(c(x1, y1,
                           x1, y2,
                           x2, y2,
                           x2, y1,
                           x1, y1), nc = 2, byrow = TRUE)))
}

split_population <- function(pts, bounds) {
  l <- bounds[1]; r <- bounds[3]; 
  b <- bounds[2]; t <- bounds[4];
  w <- r - l; h <- t - b;
  xy <- pts |> filter(x > l, x <= r, y > b, y <= t)
  bbs <- list()
  if (h > w) { # cut horizontally
    my <- weighted.median(xy$y, xy$pop_250m_grid)
    bb <- xy |> filter(y <= my)
    x1 <- min(bb$x) - 125; x2 <- max(bb$x) + 125;
    y1 <- min(bb$y) - 125; y2 <- my;
    bbs <- list.append(bbs, get_rectangle(x1, x2, y1, y2))
    bb <- xy |> filter(y > my)
    x1 <- min(bb$x) - 125; x2 <- max(bb$x) + 125;
    y1 <- my; y2 <- max(bb$y) + 125; 
    bbs <- list.append(bbs, get_rectangle(x1, x2, y1, y2))
  } else {
    mx <- weighted.median(xy$x, xy$pop_250m_grid)
    bb <- xy |> filter(x <= mx)
    x1 <- min(bb$x) - 125; x2 <- mx; 
    y1 <- min(bb$y) - 125; y2 <- max(bb$y) + 125;
    bbs <- list.append(bbs, get_rectangle(x1, x2, y1, y2))
    bb <- xy |> filter(x > mx)
    x1 <- mx; x2 <- max(bb$x) + 125;
    y1 <- min(bb$y) - 125; y2 <- max(bb$y) + 125;
    bbs <- list.append(bbs, get_rectangle(x1, x2, y1, y2))
  }
  bbs
}

bbs <- split_population(pop_grid, nz$geom[[1]] |> st_bbox())
bb_sets <- list(bbs)
for (i in 1:9) {
  next_bbs <- list()
  for (bb in (bbs |> lapply(st_bbox))) {
    next_bbs <- append(next_bbs, split_population(pop_grid, bb))
  }
  bb_sets <- append(bb_sets, list(next_bbs))
  bbs <- next_bbs
}

polygon_list_to_sf <- function(lst, crs = 2193) {
  lst |> 
    st_sfc() |>
    as.data.frame() |>
    st_sf(crs = crs)
}

eq_pop_boxes <- polygon_list_to_sf(bbs)

ggplot(nz) + 
  geom_sf(fill = "#dddddd", lwd = 0) +
  geom_tile(data = pop_grid, aes(x = x, y = y, width = 250, height = 250)) +
  geom_sf(data = eq_pop_boxes, fill = "#ff101040", 
          colour = "white", lwd = 0.15) +
  guides(fill = "none") +
  theme_void()

layer <- 1
bb_layers <- bb_sets[[1]] |>
  polygon_list_to_sf() |>
  mutate(layer = 1, n = 2)
for (i in 2:length(bb_sets)) {
  bb_layers <- bind_rows(
    bb_layers, bb_sets[[i]] |> 
      polygon_list_to_sf() |> 
      mutate(layer = i, n = 2 ^ i))
}

ggplot(nz) + 
  geom_sf(fill = "#dddddd", lwd = 0) +
  geom_sf(data = bb_layers, 
          fill = "#ff101040", colour = "white", lwd = 0.15) + 
  facet_wrap( ~ n, ncol = 5) +
  theme_void()

# tmap_mode("view")
# tm_shape(eq_pop_boxes) +
#   tm_polygons(fill = "#ff101040") +
#   tm_view(use_WebGL = FALSE)
