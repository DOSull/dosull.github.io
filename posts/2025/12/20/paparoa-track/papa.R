library(terra)
library(sf)
library(tmap)
library(dplyr)
library(stringr)
library(igraph)

# -------------------------------------------------------------

dem <- rast("nzsosdem.tif")
sea <- dem <= 0
dem <- dem |> mask(sea, maskvalues = TRUE)

slope <- dem |> terrain(unit = "radians")
aspect <- dem |> terrain(v = "aspect", unit = "radians")
shade <- shade(slope, aspect, direction = 135)

track <- st_read("paparoa-track.gpkg")
huts <- st_read("huts.gpkg")

m_base <- 
  tm_shape(sea) +
  tm_raster(
    col.scale = tm_scale_categorical(values = c("white", "lightblue2")),
    col.legend = tm_legend_hide()) +
  tm_shape(dem) + 
  tm_raster(
    col.scale = tm_scale_continuous(
      values = "hcl.terrain2", limits = c(0, 3000)),
    col_alpha = 0.6, 
    col.legend = tm_legend_hide()) +
  tm_shape(shade) +
  tm_raster(
    col.scale = tm_scale_continuous(values = "brewer.greys"),
    col_alpha = 0.2, 
    col.legend = tm_legend_hide())

m_tracks_and_huts <-
  tm_shape(track) +
  tm_lines(col = "red3", lty = "dashed", lwd = 1.5) +
  tm_shape(huts) +
  tm_dots(col = "white", fill = "red3", shape = 22, size = 1)

m_labels <-
  tm_shape(huts) +
  tm_text(
    text = "name", ymod = 0.75, size = 0.9,
    options = opt_tm_text(just = c("center", "bottom")))

m_scalebar <- 
  tm_scalebar(
    text.size = 0.75,
    position = tm_pos_out(cell.h = "center", cell.v = "bottom",
                                    pos.h = "center", pos.v = "top"))

m_base + m_tracks_and_huts + m_labels + m_scalebar

viewsheds <- list()
for (i in 1:nrow(huts)) {
  viewsheds[[huts$name[i]]] <- dem |> viewshed(huts$geom[i] |> st_coordinates(), observer = 5)
}
viewsheds <- rast(viewsheds)

m_viewsheds <- 
  tm_shape(viewsheds) +
  tm_raster(
    col.scale = tm_scale_categorical(values = c("#eeeeeea0", "#ffffff00")),
    col.legend = tm_legend_hide())

m_base + m_viewsheds + m_tracks_and_huts + m_labels

# -------------------------------------------------------------

linestring_as_matrix <- function(L) {
  (L |> st_coordinates())[, 1:2]
}

join_segments <- function(s1, s2) {
  junction <- st_intersection(s1, s2)
  along_1 <- st_line_project(s1, junction, normalized = TRUE)
  along_2 <- st_line_project(s2, junction, normalized = TRUE)
  m1 <- linestring_as_matrix(s1)
  m2 <- linestring_as_matrix(s2)
  if (along_1 == 0) {
    m1 <- apply(m1, 2, rev)
  }
  if (along_2 == 0) {
    m2 <- m2[-1, ]
  } else {
    m2 <- apply(m2, 2, rev)[-1, ]
  }
  st_linestring(rbind(m1, m2)) |> 
    st_sfc() |>
    st_set_crs(st_crs(s1))
}

segments <- track |>
  st_cast("LINESTRING")

connections <- segments |>
  st_touches()

start_finish <- which((connections |> lengths()) == 1)

sequence <- (segments |> 
               st_touches(sparse = FALSE) |>
               graph_from_adjacency_matrix(mode = "undirected") |>
               shortest_paths(from = start_finish[1], to = start_finish[2]))$vpath |>
  unlist()

single_track <- segments$geom[sequence[1]]
for (i in sequence[-1]) {
  single_track <- join_segments(single_track, segments$geom[i])
}

# -------------------------------------------------------------

viewpoints <- ((0:200) / 200) |> 
  st_line_interpolate(line = single_track, normalized = TRUE) |>
  as.data.frame() |>
  st_as_sf()
sheds <- list()
view_scope <- c()
for (i in 1:nrow(viewpoints)) {
  sheds[[i]] <- viewshed(dem, viewpoints$geometry[i] |> st_coordinates()) * 1
  view_scope <- c(
    view_scope,
    sheds[[i]] |> values() |> sum(na.rm = TRUE)
  )
}
sheds <- rast(sheds)
names(sheds) <- str_c("v", 0:200)

maps <- list()
for (i in 1:201) {
  maps[[i]] <- m_base + 
    tm_shape(sheds[[i]]) +
    tm_raster(col.scale = tm_scale_categorical(values = c("#eeeeeea0", "#ffffff00")),
              col.legend = tm_legend_hide()) + 
    m_tracks_and_huts +
    tm_shape(viewpoints |> slice(i)) +
    tm_dots(size = 1, shape = 1)
}
tmap_animation(maps, "test.mp4", width = 1000, height = 1600, delay = 20)
