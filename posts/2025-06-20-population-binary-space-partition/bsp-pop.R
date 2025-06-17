library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(spatstat)  # for weighted median function
library(rlist)     # for nicer list append option
library(data.tree)


pop_grid <- read.csv("nz-pop-grid-250m.csv") |>
  dplyr::filter(x < 2.2e6)                        # <1>
nz <- st_read("nz.gpkg")


get_rectangle <- function(xmin, ymin, xmax, ymax) {         # <1>
  st_polygon(list(matrix(c(xmin, xmin, xmax, xmax, xmin,
                           ymin, ymax, ymax, ymin, ymin), nc = 2)))
}


split_population <- function(pop_pts, bounds) {
  w <- bounds[1]; e <- bounds[3];                              # <1>
  s <- bounds[2]; n <- bounds[4];
  width <- e - w; height <- n - s;
  xy <- pop_pts |> filter(x > w, x <= e, y > s, y <= n)        # <2>
  bbs <- list()
  if (height > width) { # cut east-west at median y            # <3>
    my <- weighted.median(xy$y, xy$pop_250m_grid)
    south <- xy |> filter(y <= my)
    x1 <- min(south$x) - 125; x2 <- max(south$x) + 125;        # <4>
    y1 <- min(south$y) - 125; y2 <- my;
    bbs[[1]] <- get_rectangle(x1, y1, x2, my)
    north <- xy |> filter(y > my)
    x1 <- min(north$x) - 125; x2 <- max(north$x) + 125;
    y1 <- my; y2 <- max(north$y) + 125; 
    bbs[[2]] <- get_rectangle(x1, my, x2, y2)
  } else { # cut north-south at median x
    mx <- weighted.median(xy$x, xy$pop_250m_grid)
    west <- xy |> filter(x <= mx)
    x1 <- min(west$x) - 125; x2 <- mx;
    y1 <- min(west$y) - 125; y2 <- max(west$y) + 125;
    bbs[[1]] <- get_rectangle(x1, y1, mx, y2)
    east <- xy |> filter(x > mx)
    x1 <- mx; x2 <- max(east$x) + 125;
    y1 <- min(east$y) - 125; y2 <- max(east$y) + 125;
    bbs[[2]] <- get_rectangle(mx, y1, x2, y2)
  }
  bbs
}


bsp <- Node$new("X", bb = do.call(get_rectangle, st_bbox(nz) |> as.list()))
for (level in 1:10) {
  leaves <- bsp$leaves
  for (i in 1:length(leaves)) {
    leaf <- leaves[[i]]
    id <- leaf$name
    bbs <- split_population(pop_grid, leaf$bb |> st_bbox())
    leaf$AddChildNode(Node$new(paste(id, "1", sep = ""), bb = bbs[[1]]))
    leaf$AddChildNode(Node$new(paste(id, "2", sep = ""), bb = bbs[[2]]))
  }
}


all_levels <- bsp$Get("bb") |> 
  lapply(list) |> 
  lapply(st_polygon) |>
  st_as_sfc() |>
  as.data.frame() |>
  st_sf(crs = 2193) |>
  mutate(level = bsp$Get("level"),
         id = bsp$Get("name"),
         n = 2 ^ (level - 1))


top_level <- all_levels |> filter(level == 11) 


ggplot(nz) + 
  geom_sf(fill = "grey", lwd = 0) +
  geom_tile(data = pop_grid, aes(x = x, y = y, width = 250, height = 250)) +
  geom_sf(data = top_level, fill = "#ff101040", 
          colour = "white", lwd = 0.15) +
  guides(fill = "none") +
  theme_void()


ggplot(nz) + 
  geom_sf(fill = "grey", lwd = 0) +
  geom_sf(data = all_levels |> filter(level > 1), 
          fill = "#ff101040", colour = "white", lwd = 0.1) + 
  facet_wrap( ~ n, ncol = 5) +
  theme_void()


bb <- c(1.745e6, 5.907e6, 1.77e6, 5.926e6)
ggplot() +
  geom_sf(data = nz, fill = "white", lwd = 0) + 
  geom_tile(data = pop_grid |> filter(x >= bb[1], x <= bb[3], y >= bb[2], y <= bb[4]), 
            aes(x = x, y = y, width = 250, height = 250, fill = pop_250m_grid)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_sf(data = nz, fill = NA, colour = "#1e90ff80", lwd = 0.35) + 
  geom_sf(data = all_levels |> filter(level == 11), 
          fill = "#00000030", colour = "white", lwd = 0.2) +
  coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = FALSE, datum = 2193) +
  guides(fill = "none") +
  theme_void() +
  theme(panel.border = element_rect(fill = NA, linewidth = 1), 
        panel.background = element_rect(fill = "#1e90ff30", linewidth = 0))
