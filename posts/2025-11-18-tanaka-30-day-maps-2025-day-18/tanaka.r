library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(cols4all)
library(dbscan)
library(whitebox)
library(stars)

# from https://stac.astrogeology.usgs.gov/browser-dev/#/https://g6goyz4w56.execute-api.us-west-2.amazonaws.com/prod/collections/mro_hirise_socet_dtms/items/DTEED_050125_1480_050046_1480_G01?.asset=asset-fom_thumbnail
setwd("/Users/david/Documents/code/dosull.github.io/posts/_2025-12-xx-tanaka-contours")

dem_raw <- rast("DTEED_050125_1480_050046_1480_G01.tif")

proj <- "+proj=aeqd +lat_0=-31.46 +lon_0=-22.9 +k=1 +R=3396190 +units=m +no_defs"
dem <- dem_raw |> 
  project(proj) |>
  terra::aggregate(5)

names(dem) <- "z"

# _Of course_, there's a package for that.

library(metR)

demxy <- dem |>
  as.data.frame(xy = TRUE)

ggplot(demxy) + 
  geom_raster(aes(x = x, y = y), fill = "grey") +
  geom_contour_tanaka(
    aes(x = x, y = y, z = z),
    binwidth = 20, sun.angle = 45) + 
  coord_equal() + 
  theme_void() 

ggplot(demxy) + 
  geom_contour_fill(
    aes(x = x, y = y, z = z)) +
  # scale_fill_continuous_c4a_seq(palette = "hcl.terrain2") +
  scale_fill_continuous_c4a_seq(palette = "hcl.heat2") +
  # scale_fill_continuous_c4a_seq(palette = "-tableau.orange") +
  # scale_fill_continuous_c4a_seq(palette = "met.greek") +
  geom_contour_tanaka(
    aes(x = x, y = y, z = z), 
    binwidth = 20, sun.angle = 45) + 
  coord_equal() + 
  theme_void()

# And another one: https://github.com/riatelab/tanaka

# and here's my crude implementation

# round x to nearest y
round_to_nearest <- function(x, y) round(x / y) * y

# get contour levels for supplied dem at specified interval
contour_levels <- function(dem, interval) {
  range_z <- range(values(dem), na.rm = TRUE) |> round_to_nearest(interval)
  seq(range_z[1]- interval, range_z[2] + interval, interval)
}

levels <- contour_levels(dem, 20)
contours <- dem |> 
  st_as_stars() |> 
  st_contour(breaks = levels) |>
  st_as_sf()

contours_se <- contours |>
  mutate(geometry = geometry + c(10, -10)) |>
  st_set_crs(proj)

contours_nw <- contours |>
  mutate(geometry = geometry + c(-10, 10)) |>
  st_set_crs(proj)

g1 <- ggplot()
for (h in levels) {
  g1 <- g1 + 
    geom_sf(data = contours_se |> filter(Min == h), 
            # colour = NA, fill = "#C03D2B") +
            colour = NA, fill = "black") +
    geom_sf(data = contours_nw |> filter(Min == h), 
            # colour = NA, fill = "#D9BF77") +
            colour = NA, fill = "white") +
    geom_sf(data = contours |> filter(Min == h), 
            colour = NA, # "#E67E22", linewidth = 0.15, 
            fill = "grey")
}
g1 +
  coord_sf(datum = st_crs(contours)) + 
  theme_void()

# ggsave("tanaka.png", width = 6, height = 9.2, dpi = 300)
