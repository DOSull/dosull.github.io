library(dplyr)
library(sf)
library(pracma)
library(scales)
library(ggplot2)
library(gganimate)

get_centre <- function(m) {
  m |> 
    apply(2, range) |>
    apply(2, mean)
}

mean_distance_to_centre <- function(m, centre) {
  (m - centre) ^ 2 |>
    apply(1, sum) |>
    sqrt() |>
    mean()
}

recentre <- function(m, r = 1) {
  centre <- m |> get_centre()
  d <- mean_distance_to_centre(m, centre)
  (m - centre) * r / d + centre
}

average_corners <- function(m, x = 0.5) {
  m |>
    rbind(m[1, ]) |> 
    apply(2, conv, c(x, 1 - x)) |>
    head(-1) |>
    tail(-1)
}

matrix_as_polygon <- function(m) {
  rbind(m, m[1, ]) |> 
    list() |>
    st_polygon()
}

get_polygons <- function(n_sides, n_iterations = n_sides * 2, x = 0.5) {
  m <- 
    matrix(rnorm(2 * n_sides), ncol = 2) |>
    recentre()
  m_list <- list(m)
  for (i in 1:n_iterations) {
    m_list[[i + 1]] <- 
      m_list[[i]] |> 
      average_corners(x = x) |> 
      recentre()
  }
  m_list |> 
    lapply(matrix_as_polygon) |>
    cbind() |>
    st_as_sfc() |>
    data.frame() |> 
    st_sf()
}

drop_frames <- 50
gdf <- get_polygons(200, 500) |>
  slice(-(1:drop_frames))

ggplot(gdf) +
  geom_sf(fill = NA, colour = "#00000010") +
  theme_void()

anim <- ggplot(gdf |> mutate(t = row_number())) + 
  geom_sf(fill = NA, colour = "black") + 
  transition_manual(t) + 
  theme_void()
        
anim <- animate(anim, nframes = nrow(gdf), fps = 25)

anim_save("~/Downloads/iterated-polygons.gif", anim)
