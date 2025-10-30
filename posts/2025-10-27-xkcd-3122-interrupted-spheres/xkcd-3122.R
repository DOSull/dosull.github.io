library(sf)
library(units)
library(dplyr)
library(stringr)
library(smoothr)
library(geosphere)
library(ggplot2)

world <- st_read("world.gpkg") |>
  st_set_geometry("geometry")

ggplot() + 
  geom_sf(data = world)

get_limit_of_nsper_proj <- function(h, projstring, R = 6371008) {
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

height_for_horizon <- function(d, R = 6371008) {                    # <1> 
  R / cos(d / R) - R
}

make_local_spherical_projection <- function(lon_0, lat_0, d) {
  centre <- st_point(c(lon_0, lat_0)) |>
    st_sfc() |>
    st_set_crs(4326)
  h <- height_for_horizon(d)
  aeqd_proj <- str_glue("+proj=aeqd +lon_0={lon_0} +lat_0={lat_0}")
  nsper_proj <- str_glue("+proj=nsper +lon_0={lon_0} +lat_0={lat_0} +h={h}")
  globe_nsper <- get_limit_of_nsper_proj(h, nsper_proj)
  globe_aeqd <- globe_nsper |> 
    st_transform(aeqd_proj)
  world_nsper <-
    world |> 
    filter(drop_units(st_distance(geometry, centre)) < d) |>
    densify(20) |>
    st_transform(aeqd_proj) |>
    st_intersection(globe_aeqd) |>
    st_transform(nsper_proj)
  list(shapes = world_nsper, globe = globe_nsper)
}

make_world_map <- function(df) {
  shapes <- list()
  globes <- list()
  for (i in 1:dim(df)[1]) {
    shift <- c(df$dx[i], df$dy[i])
    a <- df$rotn[i] * pi / 180
    rot_m <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
    results <- do.call(make_local_spherical_projection, df[i, 2:4])
    shapes[[i]] <- 
      results$shapes |> 
      mutate(facet = df$facet[i], 
             geometry = (geometry * rot_m) + shift)
    globes[[i]] <- 
      results$globe |> 
      mutate(facet = df$facet[i], 
             geometry = (geometry * rot_m) + shift) 
  }
  shapes <- shapes |> bind_rows()
  globes <- globes |> bind_rows()
  outlines <- shapes |> group_by(facet) |> summarise()
  ggplot() +
    geom_sf(data = globes, fill = "grey", colour = NA) +
    geom_sf(data = shapes, fill = "white", colour = "black", lwd = 0.25) +
    geom_sf(data = outlines, fill = NA, colour = "black", lwd = 0.5) +
    geom_sf(data = globes, fill = NA, colour = "black", lwd = 0.75) +
    geom_sf_label(data = globes, aes(label = facet |> str_to_upper()), size = 4,
                 label.size = 0, fill = "#ffffff80") +
    theme_void()
}

args <- data.frame(
  facet = c("North America", "Europe", "Asia", "South America", 
            "Africa", "Antarctica", "Oceania", "Zealandia", "Australia"),
  lon_0 = c(  -88,    12,    95,   -55,    15,   -30,   180,   178,   137),
  lat_0 = c(   40,    53,    35,   -18,     0,   -85,     0,   -32,   -27),
  d     = c(4.5e6, 2.7e6, 7.0e6, 4.5e6, 5.0e6, 3.0e6, 5.0e6, 3.4e6, 3.6e6),
  dx    = c(-0.79, -0.11,  0.43, -0.67, -0.13,  -0.5,  1.08,  1.14,  0.78) * 1e7,
  dy    = c( 0.38,  0.44,  0.39, -0.08,  0.03, -0.44,  0.28, -0.16, -0.08) * 1e7,
  rotn  = c(    5,     0,   -25,    10,     0,   -15,     0,     0,    -5)
)

make_world_map(args)
ggsave("nine-continents-interrupted-spheres.png", width = 8, height = 5)
