library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(spatstat)   # for weighted median function

setwd("~/Documents/code/dosull.github.io/posts/2025-06-13-population-quadrants")

pop_grid <- read.csv("nz-pop-grid-250m.csv")
cx <- mean(pop_grid$x)
cy <- mean(pop_grid$y)
pop_grid <- pop_grid |>
  mutate(x = x - cx, y = y - cy)

nz <- st_read("nz.gpkg") |>
  mutate(geom = geom - c(cx, cy))

basemap <- ggplot(nz) + 
  geom_sf(lwd = 0) +
  geom_tile(data = pop_grid, aes(x = x, y = y, fill = pop_250m_grid)) +
  scale_fill_distiller(palette = "Reds", direction = -1) +
  theme_void() +
  theme(legend.position.inside = c(0.2, 0.7),
        legend.position = "inside")
basemap

weighted_bisector <- function(pts, angle) {
  if (angle == 0) {
    median_y <- weighted.median(pts$y, pts$pop_250m_grid)
    A <- 0; B <- 1
    # weighted median of the y's
    C <- -weighted.median(pts$y, pts$pop_250m_grid)
  } else if (angle == 90) {
    A <- 1; B <- 0
    # weighted median of the x's
    C <- -weighted.median(pts$x, pts$pop_250m_grid)
  } else {
    a <- angle * pi / 180
    rot_m <- matrix(c(cos(-a), -sin(-a), sin(-a), cos(-a)), 2, 2, byrow = TRUE)
    pts_r <- rot_m %*% matrix(c(pts$x, pts$y), nrow = 2, byrow = TRUE)
    median_y <- weighted.median(pts_r[2, ], pts$pop_250m_grid)
    A <-  median_y / cos(a)
    B <- -median_y / sin(a)
    C <- -A * B
  }
  list(A = A, B = B, C = C)
}

get_slope <- function(sl) {-sl$A / sl$B}

get_intercept <- function(sl) {-sl$C / sl$B}

get_cells_one_side_of_line <- function(pts, sl, above = TRUE) {
  if (above) {
    pts |> mutate(chosen = sl$A * x + sl$B * y + sl$C > 0) |>
      pull(chosen)
  } else {
    pts |> mutate(chosen = sl$A * x + sl$B * y + sl$C <= 0) |>
      pull(chosen)
  }
}

get_pop_one_side_of_line <- function(pts, sl, above = TRUE) {
  sum(pts$pop_250m_grid[get_cells_one_side_of_line(pts, sl, above)])
}

# sanity check
x0 <- weighted_bisector(pop_grid, 0)
basemap + geom_abline(aes(slope = -x0$A / x0$B, intercept = -x0$C / x0$B),
                linetype = "dashed", lwd = 0.5) +
  guides(fill = "none")

get_pop_one_side_of_line(pop_grid, x0)
get_pop_one_side_of_line(pop_grid, x0, FALSE)

get_quadrant_pops <- function(pts, sl1, sl2) {
  above1 <- get_cells_one_side_of_line(pts, sl1)
  above2 <- get_cells_one_side_of_line(pts, sl2)
  c(sum(pts$pop_250m_grid[ above1 &  above2]),
    sum(pts$pop_250m_grid[ above1 & !above2]),
    sum(pts$pop_250m_grid[!above1 & !above2]),
    sum(pts$pop_250m_grid[!above1 &  above2]))
}

plot_range <- function(angles) {
  bisectors      = lapply(angles     , weighted_bisector, pts = pop_grid)
  perp_bisectors = lapply(angles + 90, weighted_bisector, pts = pop_grid)
  
  df <- data.frame(angle = angles)
  df[, c("pop1", "pop2", "pop3", "pop4")] <- t(mapply(get_quadrant_pops,
                                                      bisectors,
                                                      perp_bisectors,
                                                      MoreArgs = list(pts = pop_grid)))
  
  ggplot(df |> select(angle, pop1:pop4) |> pivot_longer(cols = pop1:pop4)) +
    geom_line(aes(x = angle, y = value, group = name), lwd = 0.2, alpha = 0.35) +
    geom_point(aes(x = angle, y = value, colour = name)) +
    scale_colour_brewer(palette = "Set1", name = "Quadrant") +
    ylab("Estimated population") +
    scale_x_continuous(breaks = angles[seq(1, length(angles), 10)],
                       minor_breaks = angles) +
    theme_minimal()
}

g1 <- plot_range(-1:90) 
g1 + annotate("rect", xmin = c(-1, 76.5, 79), xmax = c(1, 78.5, 81), 
                      ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.25, lwd = 0)


g2 <- plot_range(seq(-0.5,  0.5, 0.025)) + guides(colour = "none")
g3 <- plot_range(seq(  77,   78, 0.025)) + guides(colour = "none")
g4 <- plot_range(seq(79.5, 80.5, 0.025)) + guides(colour = "none")
(g2 + g3+ plot_layout(axes = "collect")) / g4

xs <- c(79.975, 169.975) |> lapply(weighted_bisector, pts = pop_grid)
x_df <- data.frame(
  slope     = xs |> sapply(get_slope), 
  intercept = xs |> sapply(get_intercept))

basemap + 
  geom_abline(data = x_df, aes(slope = slope, intercept = intercept),
              linetype = "dashed", lwd = 0.5) +
  guides(fill = "none")

get_quadrant_pops(pop_grid, xs[[1]], xs[[2]])




weighted_n_sectors <- function(pts, cuts = 1:9/10, horizontal = TRUE, 
                               xrange = c(-Inf, Inf), yrange = c(-Inf, Inf)) {
  window <- pts |> filter(x >= xrange[1], x < xrange[2], y >= yrange[1], y < yrange[2])
  n <- length(cuts)
  df <- data.frame(h = rep(horizontal, 2 * n))
  if (horizontal) {
    intercepts <- weighted.quantile(window$y, window$pop_250m_grid, probs = cuts)
    df |> mutate(x = rep(xrange, n), y = rep(intercepts, each = 2))
  } else {
    intercepts <- weighted.quantile(window$x, window$pop_250m_grid, probs = cuts)
    df |> mutate(x = rep(intercepts, each = 2), y = rep(yrange, n))
  }
}
