library(interp)
library(tidyr)
library(dplyr)
library(ggplot2)
library(fields)

setwd("~/Documents/code/r-junkyard")

emp_proj <- read.csv("dgg-2432-no-offsets-p4-briesemeister.csv")
pts <- read.csv("world_better.csv") %>%
  dplyr::select(lon, lat)



ggplot(pts) + 
  geom_point(aes(x = lon, y = lat), size = 0.05) + 
  coord_equal()


x_out <- interp(x = emp_proj$lon, y = emp_proj$lat, z = emp_proj$x,
                xo = pts$lon, yo = pts$lat, output = "points")
y_out <- interp(x = emp_proj$lon, y = emp_proj$lat, z = emp_proj$y,
                xo = pts$lon, yo = pts$lat, output = "points")

result <- data.frame(x = x_out$z, y = y_out$z)
ggplot(result) + 
  geom_point(aes(x = x, y = y), size = 0.05) + 
  coord_equal()





spline_x <- Tps(emp_proj[, c("lon", "lat")], emp_proj$x)
spline_y <- Tps(emp_proj[, c("lon", "lat")], emp_proj$y)

splined_x <- predict(spline_x, x = pts)
splined_y <- predict(spline_y, x = pts)

result_splined <- data.frame(x = splined_x, y = splined_y)
ggplot(result_splined) + 
  geom_point(aes(x = x, y = y), size = 0.05) + 
  coord_equal()


# library(raster)
# xyz <- expand_grid(x = -180:180, y = -90:90) %>%
#   mutate(z = 0)
# r_xy <- rasterFromXYZ(xyz, crs = "+proj=longlat")
# r_splined_x <- interpolate(r_xy, spline_x)
# r_splined_y <- interpolate(r_xy, spline_y)
# 
# plot(terrain(r_splined_x))
# plot(terrain(r_splined_y))
