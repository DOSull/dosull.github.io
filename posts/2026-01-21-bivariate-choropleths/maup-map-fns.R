map_background <- function(ctx, place) {
  tm_shape(ctx, unit = "km") +
    tm_polygons(
      fill = lighten("lightgrey", 0.5),
      col = "lightblue") +
    tm_title_in(
      text = str_glue("{place} 2018 Census"),
      bg = TRUE, bg.color = "#ffffffa0",
      position = tm_pos_in(pos.h = "left", pos.v = "bottom")) +
    tm_layout(
      bg.color = desaturate("lightblue", 0.35), 
      inner.margins = rep(0, 4)
    )
}

map_boundaries <- function(df) {
  tm_shape(df) +
    tm_lines(col = "lightgrey")
}

get_palette_first_col_or_row <- function(pal = "bivario.plum_mint",
                                         column = TRUE) {
  if (column) {
    return(c4a(pal)[, 1])
  } else {
    return(c4a(pal)[1, ])
  }
}

map_one_variable <- function(ctx, place, df, var, alias,
                             pal = "bivario.plum_mint", 
                             column = TRUE) {
  map_background(ctx, place) +
    tm_shape(df) +
    tm_fill(
      fill = var,
      fill.scale = tm_scale_intervals(
        style = "quantile", n = 4, 
        values = get_palette_first_col_or_row(pal = pal, column = column)),
      fill.legend = tm_legend(
        reverse = TRUE, item.r = 0, item.width = 1.4,
        title = alias, bg.color = "#ffffff00",
        frame = FALSE, position = tm_pos_in(pos.h = "right", pos.v = "top"))) +
    map_boundaries(df)
}

map_two_variables <- function(ctx, place, df, df_areas,
                              var1, var2, alias1, alias2,
                              pal = "cols4all.pu_gn_bivs") {
  map_background(ctx, place) +
    tm_shape(df) +
    tm_fill(
      fill = tm_vars(c(var1, var2), multivariate = TRUE),
      fill.scale = tm_scale_bivariate(
        values = c4a(pal),
        scale1 = tm_scale_intervals(
          style = "quantile", n = 4, labels = paste("Q", 1:4, sep = "")),
        scale2 = tm_scale_intervals(
          style = "quantile", n = 4, labels = paste("Q", 1:4, sep = ""))),
      fill.legend = tm_legend_bivariate(
        frame = FALSE, text.size = 0.8, bg.color = "#ffffff00",
        position = tm_pos_in(pos.h = "right", pos.v = "top"),
        item.r = 0, item.width = 1.4, item.height = 1.4,
        xlab = alias2, ylab = alias1)) +
    map_boundaries(df_areas)
}

get_coords <- function(n, limits) {
  base_coords <- 1:(n * 2 + 1) |> scales::rescale(to = limits)
  base_coords[seq(2, n * 2, 2)]
}

get_palette_grid <- function(pal, n = NA, 
                             show_text = TRUE, textsize = 7, 
                             limits = NULL) {
  colour_matrix <- c4a(pal, n)
  if (is.null(limits)) {
    xs <- 1:ncol(colour_matrix)
    ys <- 1:nrow(colour_matrix)
  } else {
    xs <- get_coords(nrow(colour_matrix), limits)
    ys <- get_coords(ncol(colour_matrix), limits)
  }
  expand_grid(x = xs, y = ys) |>
    mutate(hex = c(colour_matrix)) |>
    ggplot() +
    geom_raster(aes(x = x, y = y, fill = hex)) +
    scale_fill_identity() +
    coord_equal() +
    ggtitle(ifelse(show_text, pal, "")) +
    theme_void() +
    theme(plot.title = element_text(size = textsize, hjust = 0.5))
}
