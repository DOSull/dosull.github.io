library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(geosphere)
library(stringdist)
library(smoothr)

airports <- read.csv("https://davidmegginson.github.io/ourairports-data/airports.csv") |> 
  filter(type %in% c("large_airport"), iata_code != "") |>
  select(name, iata_code, longitude_deg, latitude_deg) |>
  rename(x = longitude_deg, 
         y = latitude_deg) |>
  select(name, iata_code, x, y)

routes <- combn(airports$iata_code, 2) |> 
  matrix(ncol = 2, byrow = TRUE, dimnames = list(NULL, paste("iata", 1:2, sep = ""))) |> 
  as.data.frame() |>
  filter(stringdist(iata1, iata2, method = "hamming") == 1) |>
  left_join(airports, by = join_by(iata1 == iata_code)) |>
  rename(name1 = name, x1 = x, y1 = y) |>
  left_join(airports, by = join_by(iata2 == iata_code)) |>
  rename(name2 = name, x2 = x, y2 = y) |>
  select(iata1, iata2, name1, name2, x1, y1, x2, y2)

c_meridian <- 30
projection <- str_glue("+proj=natearth lon_0={c_meridian}")

routes_sf <- gcIntermediate(routes |> select(x1:y1), routes |> select(x2:y2), 
                            n = 100, breakAtDateLine = TRUE,
                            addStartEnd = TRUE, TRUE, sp = TRUE) |>
  st_as_sf() |>
  bind_cols(routes) |>
  st_break_antimeridian(lon_0 = c_meridian)

world <- spData::world |> 
  st_break_antimeridian(lon_0 = c_meridian) |>
  st_transform(projection)

globe <- st_polygon(
  list(matrix(c(c(-179.999, -179.999, 179.999, 179.999, -179.999) + 
                  c_meridian, c( -90,   90,  90, -90,  -90)), ncol = 2))) |>
  st_sfc() |>
  as.data.frame() |>
  st_sf(crs = 4326) |>
  densify(100) |>
  st_transform(projection)

ggplot() +
  geom_sf(data = globe, fill = "#ddeeff", colour = NA) +
  geom_sf(data = world, fill = "#dddddd", colour = NA) +
  geom_sf(data = routes_sf, lwd = 0.05, colour = "#666666") +
  theme_void()
  
scheduled <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat", header = FALSE) |>
  select(3, 5) |> 
  rename(iata1 = V3, iata2 = V5) |>
  mutate(AB = str_c(iata1, "-", iata2), 
         BA = str_c(iata2, "-", iata1))

routes_sf_actual <- routes_sf |>
  mutate(AB = str_c(iata1, "-", iata2)) |>
  filter(AB %in% scheduled$AB | AB %in% scheduled$BA)

airport_labels <- airports |>
  filter(iata_code %in% c(routes_sf_actual$iata1, routes_sf_actual$iata2)) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_transform(projection)

ggplot() +
  geom_sf(data = globe, fill = "#ddeeff", colour = NA) +
  geom_sf(data = world, fill = "#dddddd", colour = NA) +
  geom_sf(data = routes_sf, lwd = 0.05, colour = "#666666") +
  geom_sf(data = routes_sf_actual, lwd = 0.5, colour = "red") +
  # geom_sf_text(data = airport_labels, aes(label = iata_code), size = 2) +
  theme_void()
