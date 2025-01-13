library(spacefillr)
library(spatstat)
library(spbal)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(cols4all)

set.seed(1)

# projection <- "+proj=laea"
# aspect_ratio <- 1
projection <- "+proj=moll"
aspect_ratio <- 2

r <- 6371000
angles <- 0:719 / 720 * 2 * pi
angles <- c(angles, angles[1])
x <- r * cos(angles) * 2 * sqrt(aspect_ratio)
y <- r * sin(angles) * 2 / sqrt(aspect_ratio)

globe <- c(x, y) |> 
  matrix(ncol = 2) |>
  list() |>
  st_polygon() |>
  st_sfc() |>
  st_as_sf(crs = projection)

world <- ne_countries() |>
  select() |>
  st_transform(projection)

abs_diffs <- function(v1, v2) {
  outer(v1, v2, "-") |> abs()
}

toroidal_squared_distances <- function(m1, m2) {
  dx <- abs_diffs(m1[, 1], m2[, 1])
  dy <- abs_diffs(m1[, 2], m2[, 2])
  dx <- pmin(dx, 1 - dx)
  dy <- pmin(dy, 1 - dy)
  dx * dx + dy * dy
}

add_remote_point <- function(m, n_choices = 2) {
  # print(m)
  n_candidates <- nrow(m) * n_choices
  candidates <- runif(n_candidates * 2) |> 
    matrix(ncol = 2)
  # print(candidates)
  r_max <- toroidal_squared_distances(candidates, m) |> 
    apply(1, min) |> 
    which.max()
  # print(r_max)
  rbind(m, candidates[r_max, ])
}

spaced_points <- function(n = 50, n_choices = 5) {
  points <- runif(2) |> matrix(ncol = 2)
  for (i in 2:n) {
    points <- add_remote_point(points, n_choices = n_choices)
  }
  points |> 
    as.data.frame() |>
    rename(x = V1, y = V2)
}

n_points <- 512 

df0 <- spaced_points(n = n_points, n_choices = 2) |>
  mutate(gen = "Naive method")

df1 <- generate_sobol_owen_set(n_points, 2) |>
  as.data.frame() |>
  rename(x = V1, y = V2) |>
  mutate(gen = "Sobol-Owen")

df2 <- cppRSHalton_br(n_points)$pts |>
  as.data.frame() |>
  rename(x = V1, y = V2) |>
  mutate(gen = "Halton")

df3 <- rstrat(nx = 2 * sqrt(n_points / 2), 
              ny = sqrt(n_points / 2), k = 1) |>
  as.data.frame() |>
  mutate(gen = "Stratified random")

df4 <- runif(n_points * 2) |>
  matrix(ncol = 2) |> 
  as.data.frame() |>
  rename(x = V1, y = V2) |>
  mutate(gen = "Random uniform")

df5 <- rSSI(r = 2.25 / pi / sqrt(n_points), n = n_points) |>
  as.data.frame() |>
  mutate(gen = "Sequential Spatial Inhibition (SSI)")

all_points <- bind_rows(df0, df1, df2, df3, df4, df5) |>
  mutate(gen = ordered(gen, c("Random uniform", 
                              "Stratified random", "Naive method", 
                              "Sequential Spatial Inhibition (SSI)", 
                              "Halton", "Sobol-Owen"))) |>
  mutate(x = x * 360 - 180, y = acos(y * 2 - 1) * 180 / pi - 90) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_transform(projection)

ggplot(globe) +
  geom_sf(fill = "#cceeff", linewidth = 0) +
  geom_sf(data = world, fill = "grey", linewidth = 0) +
  geom_sf(data = all_points, size = 0.5, colour = "red") +
  facet_wrap( ~ gen, ncol = ifelse(aspect_ratio > 1, 2, 3)) +
  theme_void()

xy <- all_points |>
  st_coordinates() |>
  as.data.frame() |>
  bind_cols(all_points)

nx <- sqrt(n_points / pi) * 2 * sqrt(aspect_ratio)
ny <- nx * 2 / sqrt(3) / sqrt(aspect_ratio)

ggplot(xy) +
  geom_hex(aes(x = X, y = Y, fill = as.factor(after_stat(count))), 
           bins = c(nx, ny), linewidth = 0.1, colour = "#666666") +
  scale_fill_manual(values = c4a("brewer.yl_gn_bu"),
                    breaks = 1:6, guide = "legend", name = "count") +
  annotate(geom = "path", x = x, y = y, linewidth = 0.2) +
  coord_equal() +
  facet_wrap( ~ gen, ncol = ifelse(aspect_ratio > 1, 2, 3)) +
  theme_void()
