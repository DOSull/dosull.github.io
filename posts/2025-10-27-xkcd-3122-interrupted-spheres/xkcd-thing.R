library(sf)
library(units)
library(dplyr)
library(stringr)
library(smoothr)
library(ggplot2)

world <- st_read("world.gpkg") |>
  st_set_geometry("geometry")

distance_to_horizon <- function(h, R = 6371008) {
  R * acos(R /(h + R))
}

height_for_horizon <- function(d, R = 6371008) {
  R / cos(d / R) - R
}

get_limit_of_nsper_projection <- function(h, projstring, R = 6371008) {
  P <- h / R + 1
  r <- R * sqrt((P - 1) / (P + 1))
  st_point(c(0, 0)) |>
    st_sfc() |> 
    data.frame() |>
    st_sf() |>
    st_buffer(r) |>
    densify(20) |>
    st_set_crs(projstring)
}

make_local_spherical_projection <- function(facet, lon_0, lat_0, d) {
  centre <- st_point(c(lon_0, lat_0)) |> 
    st_sfc() |> 
    st_set_crs(4326)
  h <- height_for_horizon(d)
  aeqd_proj <- str_glue("+proj=aeqd +lon_0={lon_0} +lat_0={lat_0}")
  nsper_proj <- str_glue("+proj=nsper +lon_0={lon_0} +lat_0={lat_0} +h={h}")
  globe_nsper <- get_limit_of_nsper_projection(h, nsper_proj)
  # globe_aeqd <- globe_nsper |> 
  #   st_transform(aeqd_proj)
  world_nsper <-
    world |> 
    filter(drop_units(st_distance(geometry, centre)) < d) |>
    densify(20) |>
    st_transform(aeqd_proj) |>
    st_intersection(globe_aeqd) |>
    st_transform(nsper_proj)
  list(shapes = world_nsper, globes = globe_nsper)
}

make_world_map <- function(df) {
  shapes <- list()
  globes <- list()
  for (i in 1:dim(df)[1]) {
    shift <- c(df$dx[i], df$dy[i])
    a <- df$rotn[i] * pi / 180
    rot_m <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
    results <- do.call(make_local_spherical_projection, df[i, 1:4])
    shapes[[i]] <- 
      results$shapes |> 
      mutate(facet = df$facet[i], 
             geometry = (geometry * rot_m) + shift)
    globes[[i]] <- 
      results$globes |> 
      mutate(facet = df$facet[i], 
             geometry = (geometry * rot_m) + shift) 
  }
  shapes <- 
    shapes |> 
    bind_rows()
  globes <- 
    globes |> 
    bind_rows()
  outlines <-
    shapes |>
    group_by(facet) |>
    summarise()
  ggplot() +
    geom_sf(data = globes, fill = "grey", colour = NA) +
    geom_sf(data = shapes, fill = "white", colour = "black", lwd = 0.25) +
    geom_sf(data = outlines, fill = NA, colour = "black", lwd = 0.5) +
    geom_sf(data = globes, fill = NA, colour = "black", lwd = 0.75)
}

args <- data.frame(
  facet = c("North America", "Europe", "Asia", "South America", 
            "Africa", "Antarctica", "Oceania"),
  lon_0 = c(  -85,    12,    95,   -55,    15,    60,   150),
  lat_0 = c(   38,    53,    35,   -10,     5,   -85,   -30),
  d     = c(4.0e6, 2.7e6, 7.0e6, 3.8e6, 4.5e6, 3.0e6, 3.5e6),
  dx    = c(-0.73, -0.18,  0.38, -0.62, -0.17,  0.47,  0.74) * 1e7,
  dy    = c(  0.2,  0.33,  0.27, -0.19, -0.05, -0.37, -0.17) * 1e7,
  rotn  = c(    5,     0,   -25,    10,     0,     0,     5)
)

make_world_map(args)

args2 <- data.frame(
  facet = 1:12,
  lon_0 = c(0, -144, -72, 0, 72, 144, -108, -36, 36, 108, 180, 36),
  lat_0 = c(90, 30, 30, 30, 30, 30, -30, -30, -30, -30, -30, -90),
  d     = rep(6e6, 12),
  dx    = c(3, 1:5, 1:5+0.5, 3.5) * 7.7e6,
  dy    = c(1.71, rep(0.5, 5), rep(-0.5, 5), -1.71) * 5.31e6,
  rotn  = rep(0, 12)
)

make_world_map(args2)

