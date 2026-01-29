library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(gstat)
library(sfdep)
library(spopt)
library(ggplot2)

setwd(dirname(rstudioapi::documentPath()))

regionalise <- function(df, n, ...) {
  vars <- list(...) |> unlist()
  if (length(vars) == 0) {
    return(df |> 
             mutate(x = rnorm(n())) |>
             azp(attrs = c("x"), n_regions = n, 
                 weights = "rook", bridge_islands = TRUE) |>
             select(-x))
  } else {
    return(df |> 
             azp(attrs = vars, n_regions = n, 
                 weights = "rook", bridge_islands = TRUE))
  }
}

dissolve <- function(df, group_var, vars, fn = sum) {
  df |>
    group_by({{ group_var }}) |>
    summarise(across({{ vars }}, ~ fn(., na.rm = TRUE)))
}

get_regionalisation <- function(name, width = 32, height = 32) {
  L <- list(expand_grid(x = 1:width, y = 1:width) |>
              mutate(z = runif(n())) |>
              rast(type = "xyz", crs = "+proj=eqc") |>
              as.polygons(aggregate = FALSE) |>
              st_as_sf() |>
              regionalise(32) |>
              pull(.region) |>
              as.integer())
  names(L) <- name
  L
}

if (file.exists("regionalisations.csv")) {
  regionalisations <- read.csv("regionalisations.csv")
} else {
  regionalisations <- paste("regions", 1:50, sep = "") |> 
    lapply(get_regionalisation) |>
    data.frame()
  regionalisations |> write.csv("regionalisations.csv", row.names = FALSE)
}
  
get_xyz_surfaces <- function(n = 1, width = 32, height = 32, range = 5) {
  xy <- expand_grid(x = 1:width, y = 1:height)
  g <- gstat(formula = z ~ 1,
             locations = ~ x + y,
             dummy = TRUE, 
             beta = 10, 
             model = vgm(psill = 0.1, range = range, model='Exp'),
             nmax = 20)
  predict(g, newdata = xy, nsim = n) |>
    rast(type = "xyz", crs = "+proj=eqc") |>
    as.polygons(aggregate = FALSE) |>
    st_as_sf()
}

grid <- get_xyz_surfaces()
nb <- st_contiguity(grid, queen = FALSE)
wt <- st_weights(nb)

mapply(get_xyz_surfaces, range = c(0.1, 1:5), SIMPLIFY = FALSE) |>
  lapply(pull, "sim1") |>
  lapply(global_moran_test, nb, wt)


get_correlations <- function(g, nsim, ij) {
  (g |> 
     select(starts_with("sim")) |> 
     st_drop_geometry() |> 
     cor())[ij]
}

nsim <- 50
ij <- combn(1:nsim, 2) |> t()
ij_df <- ij |> 
  data.frame() |> 
  rename(i = X1, j = X2)
ranges <- c(0.1, 1, 2, 5, 10, 20)

dfs <- list()
for (range in ranges) {
  grid <- get_xyz_surfaces(n = nsim, range = range) |>
    mutate(region = regionalisations$regions1)
  grid32 <- grid |>
    dissolve(group_var = region, vars = starts_with("sim"), mean)
  dfs[[length(dfs) + 1]] <- 
    ij_df |>
    mutate(c0 = grid   |> get_correlations(nsim, ij), 
           c1 = grid32 |> get_correlations(nsim, ij),
           range = range)
}
df_by_range <- bind_rows(dfs)

ggplot(df_by_range, aes(x = c0, y = c1)) + 
  geom_hex(binwidth = 0.08) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  # geom_smooth(colour = "black", linewidth = 0.5, fill = NA) +
  coord_equal() +
  xlab("Initial correlation") +
  ylab("Aggregated correlation") +
  guides(fill = "none") +
  facet_wrap( ~ range, labeller = "label_both") +
  theme_minimal()


