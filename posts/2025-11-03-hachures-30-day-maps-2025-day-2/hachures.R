library(sf)
library(terra)
library(dplyr)
library(stringr)
library(ggplot2)

setwd("~/Documents/code/dosull.github.io/posts/2025-11-03-gia-chapter-1B-part-4B")

round_to_nearest <- function(x, y) round(x / y) * y

contour_levels <- function(dem, interval) {
  range_z <- range(values(dem)) |> round_to_nearest(interval)
  seq(range_z[1], range_z[2], interval)
}

get_contour_pts <- function(dem, spacing = 10, contour_interval = 5) {
  range_z <- round_to_nearest(dem |> values() |> range(), contour_interval)
  contour_lines <- dem |> 
    as.contour(levels = contour_levels(dem, contour_interval)) |>
    st_as_sf() |>
    rmapshaper::ms_explode() |>
    mutate(length = st_length(geometry) |> units::drop_units())
  
  lines        <- contour_lines |> pull(geometry) |> lapply(st_sfc)
  line_lengths <- contour_lines |> pull(length)
  
  mapply(
    st_line_interpolate, 
    lines, 
    mapply(seq, 0, line_lengths, spacing)) |>
    do.call(what = c) |>
    data.frame() |>
    st_sf(crs = 2193)
}

get_grid_pts <- function(dem) {
  dem |>
    as.points() |>
    st_as_sf()
}

get_geomorph_df <- function(pts, slope, aspect, hillshade) { 
  dx <- sin(aspect) * tan(slope)
  names(dx) <- "dx"
  dy <- cos(aspect) * tan(slope)
  names(dy) <- "dy"
  pts |> 
    st_coordinates() |>
    data.frame() |> 
    rename(x = X, y = Y) |>
    bind_cols(extract(slope, pts, ID = FALSE)) |>
    bind_cols(extract(dx, pts, ID = FALSE)) |>
    bind_cols(extract(dy, pts, ID = FALSE)) |>
    bind_cols(extract(hillshade, pts, ID = FALSE))
}

get_hachure_points <- function(df) {
  df |> 
    st_as_sf(coords = c("x", "y"), remove = FALSE) |>
    st_set_crs(2193)
}

get_hachure_vectors <- function(df, 
                                         scale = 25,
                                         slip = 0) {
  offsets <- runif(nrow(df), 1 - slip, 1)
  df |> 
    mutate(
      offset = offsets,
      x0 = x - dx * scale * offset,
      x1 = x + dx * scale * (1 - offset),
      y0 = y - dy * scale * offset,
      y1 = y + dy * scale * (1 - offset)) |>
    select(x0, x1, y0, y1) |>
    apply(1, matrix, ncol = 2, simplify = FALSE) |>
    lapply(st_linestring) |>
    st_sfc() |>
    as.data.frame() |>
    st_sf(crs = 2193) |>
    bind_cols(df)
}


dem <- rast("zealandia-5m.tif")
cellsize <- res(dem)[1]
# this DEM has a row of NAs so get rid of them
dem <- dem |> crop(ext(dem) + c(0, 0, -cellsize, 0))

# smooth the DEM - this helps with the overall look of hachures
gauss <- focalMat(dem, 4, "Gauss")
dem    <- dem    |> focal(gauss, mean, expand = TRUE)

# get slope and aspect
slope  <- dem |> terrain(unit = "radians")
aspect <- dem |> terrain(v = "aspect", unit = "radians")
hillshade <- shade(slope, aspect, direction = 135, normalize = TRUE)

# trim all to the same size
dem <- dem |> crop(ext(dem) + rep(-cellsize, 4))
slope <- slope |> crop(dem)
aspect <- aspect |> crop(dem)
hillshade <- hillshade |> crop(dem)

# get centre of DEM (useful for trimming map to restricted extent)
cxy <- dem |>
  ext() |>
  matrix(ncol = 2) |>
  apply(2, mean)

# points at which field is sampled
points <- get_contour_pts(dem, spacing = 10, contour_interval = 5)
## points <- get_grid_pts(dem)

# augment with various geomorphic attributes
df <- get_geomorph_df(points, slope, aspect, hillshade)

# make vector linestrings
vecs   <- get_hachure_vectors(df, 30, slip = 0.0)

# plot
ggplot() +
  geom_sf(
    data = as.contour(dem, levels = contour_levels(dem, 5)) |> st_as_sf(),
    linewidth = 0.25, colour = "grey") +
  geom_sf(data = vecs, aes(linewidth = slope ^2, colour = hillshade)) +
  scale_linewidth_identity() +
  scale_colour_distiller(palette = "Greys", direction = 1) +
  guides(colour = "none") +
  coord_sf(xlim = cxy[1] + c(-500, 500),
           ylim = cxy[2] + c(-500, 500)) +
  theme_void()

get_hachure_points(df) |> st_write("~/Downloads/hachure_points.gpkg", delete_dsn = TRUE) 

## geometry generator for points:
# make_triangle(
#   make_point($x - 25 * "dx", $y - 25 * "dy"),
#   make_point($x - 1 * "dy", $y + 1 * "dx"),
#   make_point($x + 1 * "dy", $y - 1 * "dx")
# )

streams   <- st_read("streams.gpkg") |> select()
lake      <- st_read("lake.gpkg") |> select(name)

