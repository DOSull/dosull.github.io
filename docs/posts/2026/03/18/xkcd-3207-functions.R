## ===== mercator functions
R <- 6378137
get_lon <- function(x, r = R) { x / r * 180/pi }
get_lat <- function(y, r = R) { 90 - 2 * atan(exp(-y / r)) * 180/pi }
get_x <- function(lon, r = R) { r * lon * pi/180 }
get_y <- function(lat, r = R) { r * asinh(tan(lat * pi/180)) }

## ===== affine transformations
# affine rotation 2x2 matrix
rotate_m <- function(a) {
  ca <- cos(a * pi/180)
  sa <- sin(a * pi/180)
  matrix(c(ca, -sa, sa, ca), 2, 2)
}

# rotate (sf) shape about an origin point by
# post multiplication
rotate_shape <- function(s, a, xo, yo) {
  (s - c(xo, yo)) * rotate_m(-a) + c(xo, yo)
}

# rotate a point (2-element vector) about a point
project_point <- function(x, y, a, xo = 0, yo = 0) {
  rotate_m(a) %*% c(x - xo, y - yo) + c(xo, yo)
}

# rotate a point (2-element vector) about a point
project_point_2 <- function(x, y, a, xo = 0, yo = 0) {
  xp <- x + get_lat(y) * R * pi/180 * sin(a * pi/180)
  yp <- y
  c(xp, yp)
}

## ===== straight line functions
# Ax + By + C = 0 representation of straight
# line between two points
get_line_between_pts <- function(p1, p2) {
  c(A = p1[2] - p2[2], 
    B = p2[1] - p1[1],
    C = p1[1] * p2[2] - p2[1] * p1[2])
}

# Ax + By + C = 0 straight line from point and slope
get_line_with_pt_and_angle <- function(p, slope) {
  px <- p + c(cos(slope), sin(slope))
  get_line_between_pts(p, px)
}

# intersection of two Ax + By + C = 0
# straight lines as two element vector
get_intersection <- function(L1, L2) {
  eps <- .Machine$double.eps ^ 0.5
  y  <- x <- NA
  denom <- L1[1] * L2[2] - L2[1] * L1[2]
  if (abs(denom) < eps) { return(c(x, y)) }
  if      (abs(L1[1]) < eps) { y <- -L1[3] / L1[2] } 
  else if (abs(L2[1]) < eps) { y <- -L2[3] / L2[2] } 
  else if (abs(L1[2]) < eps) { x <- -L1[3] / L1[1] } 
  else if (abs(L2[2]) < eps) { x <- -L2[3] / L2[1] } 
  c(ifelse(!is.na(x), x, (L1[2] * L2[3] - L2[2] * L1[3]) / denom),
    ifelse(!is.na(y), y, (L1[3] * L2[1] - L2[3] * L1[1]) / denom))
}

## ===== make grid of points
# returns row, col, x, y, lon, lat, declination (D) data frame
# x y coordinates are Mercator projected and grid is equally 
# spaced under this projection
get_point_grid <- function(xsteps, ysteps, 
                           lon0 = -180, lon1 = 180,
                           lat0 = -85, lat1 = 85) {
  xs <- seq(lon0, lon1, length.out = xsteps * 2 + 1) |> get_x()
  ys <- seq(get_y(lat0), get_y(lat1), length.out = 2 * ysteps + 1)
  # every second coordinate for our graticule cell centres
  xcs <- xs[1:xsteps * 2]
  ycs <- ys[1:ysteps * 2]
  # point grid at cell centres
  xy_points <- expand_grid(x = xcs, y = ycs) |>
    mutate(lon = get_lon(x), lat = get_lat(y),
           row = rep(1:ysteps, xsteps),
           col = rep(1:xsteps, each = ysteps),
           name = str_glue("r{row}c{col}")) |>
    select(name, row, col, x, y, lon, lat)
  # add declinations
  xy_points <- bind_cols(
    xy_points, 
    mapply(
      igrf, year = 2025, altitude = 0,
      longitude = xy_points$lon, latitude = xy_points$lat,
      SIMPLIFY = FALSE) |>
      bind_rows()) |>
    select(name, row, col, x, y, lon, lat, D)
  list(grid = xy_points, xs = xs, ys = ys)  
}

## ===== make graticule
# make lists of points for the parallels and meridians
get_parallels_and_meridians <- function(grid) {
  xms <- grid$xs[seq(1, 2 * xsteps + 1, 2)]
  yps <- grid$ys[seq(1, 2 * ysteps + 1, 2)]
  xcs <- grid$grid$x |> unique() |> sort()
  ycs <- grid$grid$y |> unique() |> sort()
  xyc_parallels <- yps |> lapply(\(p) rbind(x = xcs, y = p))
  xyc_meridians <- xms |> lapply(\(p) rbind(x = p, y = ycs))
  # declinations
  llc_parallels <- xyc_parallels |>
    lapply(\(xy) rbind(get_lon(xy[1,]), get_lat(xy[2,])))
  llc_meridians <- xyc_meridians |>
    lapply(\(xy) rbind(get_lon(xy[1,]), get_lat(xy[2,])))
  # get declinations using igrf::igrf
  get_D <- function(lon_lat) {
    igrf(year = 2025, altitude = 0,
         longitude = lon_lat[1], latitude = lon_lat[2]) |>
      pull(D)
  }
  dec_parallels <- llc_parallels |>
    lapply(\(ll) apply(ll, 2, get_D)) |>
    lapply("*", pi/180)
  dec_meridians <- llc_meridians |>
    lapply(\(ll) apply(ll, 2, get_D)) |>
    lapply("*", pi/180)
  list(p_xy = xyc_parallels, p_d = dec_parallels,
       m_xy = xyc_meridians, m_d = dec_meridians)
}

## ===== graticule cells
# cells are 2 row (x,y) by 4 column (SW, SE, NE, SW) matrix
# graticule is a 2 row (4 x n cells) matrix
get_cell <- function(r, c, deformed = TRUE) {
  if (deformed) {
    W <- get_line_with_pt_and_angle(graticule$m_xy[[c  ]][, r],
                                    graticule$m_d[[c  ]][  r] + pi/2)
    S <- get_line_with_pt_and_angle(graticule$p_xy[[r  ]][, c],
                                    graticule$p_d[[r  ]][  c])  
    E <- get_line_with_pt_and_angle(graticule$m_xy[[c+1]][, r],
                                    graticule$m_d[[c+1]][  r] + pi/2)
    N <- get_line_with_pt_and_angle(graticule$p_xy[[r+1]][, c],
                                    graticule$p_d[[r+1]][  c])
  } else {
    W <- get_line_with_pt_and_angle(graticule$m_xy[[c  ]][, r], pi/2)
    S <- get_line_with_pt_and_angle(graticule$p_xy[[r  ]][, c], 0)  
    E <- get_line_with_pt_and_angle(graticule$m_xy[[c+1]][, r], pi/2)
    N <- get_line_with_pt_and_angle(graticule$p_xy[[r+1]][, c], 0)
  }
  cell <- cbind(get_intersection(S, W), get_intersection(S, E),
                get_intersection(N, E), get_intersection(N, W))
  dimnames(cell) <- list(c("x", "y"), c("SW", "SE", "NE", "NW"))
  cell
}

# for ease of access of this set of points, or later projected points
# this assumes 2 x (xsteps * ysteps * 4) matrix arranged by rows 
get_index <- function(row, col) { 1:4 + ((row - 1) * xsteps + (col - 1)) * 4 }

# get the 2x4 matrix of points at a give row-col location
get_cell_corners <- function(m, row, col) {
  m[, get_index(row, col), drop = FALSE]
}

# update the supplied matrix with new points at the row-col location
update_cell_corners <- function(m, new_pts, row, col) {
  m[, get_index(row, col)] <- new_pts
  m
}

# graticule cells matrix as a dataframe
cells_as_dataframe <- function(cells) {
  cells |> t() |>
    as.data.frame() |> 
    mutate(
      id = rep(1:(ncol(cells) / 4), each = 4),
      which_corner = rep(c("SW", "SE", "NE", "NW"), ncol(cells) / 4))
}

# plot cells as a set of polygons
plot_cells <- function(cells) {
  cells |> cells_as_dataframe() |>
    ggplot() + 
    geom_polygon(aes(x = x, y = y, group = id), 
                 fill = "#ff000040", colour = "black", lwd = 0.1) +
    coord_equal() +
    theme_void()
}

## ===== coordinate geometry of cells
# slopes of cell in S E N W order
get_slopes <- function(m) {
  slopes <- cbind(m, m[, 1]) |> 
    apply(1, diff) |>
    apply(1, \(x) atan2(x[2], x[1]))
  names(slopes) <- c("S", "E", "N", "W")
  slopes
}

# get opposite direction to N S E W
get_opposite_direction <- function(d) {
  ifelse(d %in% c("N", "S"), 
         ifelse(d == "N", "S", "N"),
         ifelse(d == "E", "W", "E"))
}

# lookup values for scaling functions
scale_lookups <- list(
  S = list(E = c(2, 1, 3, 4, 3, 4), 
           W = c(1, 2, 4, 3, 3, 2)),
  N = list(E = c(3, 4, 2, 1, 1, 4), 
           W = c(4, 3, 1, 2, 1, 2)),
  E = list(N = c(3, 2, 4, 1, 4, 1), 
           S = c(2, 3, 1, 4, 4, 3)),
  W = list(N = c(4, 1, 3, 2, 2, 1), 
           S = c(1, 4, 2, 3, 2, 3))
)
# scale cell supplied in m, parallel to scaled_side
# while keeping fixed_side unchanged 
# NOTE: there may be an easier way to do this not by scaling 
# the 'scaled side' but by assigning it the coordinates of
# the matching side on the target cell, shifting all other
# corners at the same time by the same amount, and then
# moving the fixed corner back to its starting relative
# position, and deriving the fourth corner as currently
# Describing it like that, perhaps on reflection it's not
# any easier!
scale_carefully <- function(m, s, scaled_side, fixed_side) {
  ref_points <- scale_lookups[[scaled_side]][[fixed_side]]
  origin     <- ref_points[1]
  end_pt     <- ref_points[2]
  fixed_pt   <- ref_points[3]
  derived_pt <- ref_points[4]
  opp_scaled <- ref_points[5]
  opp_fixed  <- ref_points[6]
  # shift to origin and scale
  m2 <- (diag(s, 2) %*% (m - m[, origin])) + m[, origin]
  # put fixed point back
  m2[, fixed_pt] <- m[, fixed_pt]
  grads <- get_slopes(m)
  opposite_scaled <- get_line_with_pt_and_angle(m2[, fixed_pt], grads[opp_scaled])
  opposite_fixed  <- get_line_with_pt_and_angle(m2[, end_pt], grads[opp_fixed])
  m2[, derived_pt] <- get_intersection(opposite_scaled, opposite_fixed)
  rownames(m2) <- c("x", "y")
  m2
}

# edge lengths of supplied 2x4 matrix in S E N W order 
get_edge_lengths <- function(m) {
  lens <- cbind(m, m[, 1]) |>
    apply(1, diff) |> 
    apply(1, \(x) sqrt(sum(x ^ 2)))
  names(lens) <- c("S", "E", "N", "W")
  lens
}

# match this cell to target cell along side_to_scale, while
# keeping side_to_fix unchanged
match_cell_scale <- function(this, target, side_to_scale, side_to_fix) {
  target_side <- get_opposite_direction(side_to_scale)
  if (!any(is.na(target))) {
    edge_lengths <- get_edge_lengths(this)
    target_edge_lengths <- get_edge_lengths(target)
    scale_factor <- 
      target_edge_lengths[target_side] / edge_lengths[side_to_scale]
    this <- scale_carefully(this, scale_factor, side_to_scale, side_to_fix)
  }
  this
}

# match this cell to target cell along side_to_scale, while
# keeping side_to_fix unchanged
match_cell_posn <- function(this, target, dir) {
  corner_pairs <- switch(dir, 
                         W = c("SE", "SW"), 
                         E = c("SW", "SE"),
                         N = c("SE", "NE"), 
                         S = c("NE", "SE"))
  if (!any(is.na(target))) {
    this <- this + target[, corner_pairs[1]] - 
      this[, corner_pairs[2]]
  }
  this
}
