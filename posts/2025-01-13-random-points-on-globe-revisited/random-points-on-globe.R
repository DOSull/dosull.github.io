library(ggplot2)
library(dplyr)
library(tidyr)

abs_diffs <- function(v1, v2) {                           # <1>
  outer(v1, v2, "-") |> abs()
}

toroidal_manhattan_distances <- function(m1, m2) {
  dx <- abs_diffs(m1[, 1], m2[, 1])
  dy <- abs_diffs(m1[, 2], m2[, 2])
  dx <- pmin(dx, 1 - dx)                                  # <2>
  dy <- pmin(dy, 1 - dy)
  dx * dx + dy * dy                                                 # <3>                   
}

add_remote_point <- function(m, choice_scaling = 1.5) {   # <4>
  n_candidates <- ceiling(log(nrow(m) * exp(1) * choice_scaling)) # <5>
  candidates <- runif(n_candidates * 2) |> 
    matrix(ncol = 2)
  r_max <- toroidal_manhattan_distances(candidates, m) |> 
    apply(1, min) |>                                      # <6>
    which.max()
  rbind(m, candidates[r_max, ])
}

spaced_points <- function(n = 50, choice_scaling = 1.5) {
  points <- runif(2) |> matrix(ncol = 2)                  # <7>
  for (i in 2:n) {
    points <- add_remote_point(points, choice_scaling = choice_scaling)
  }
  points |> 
    as.data.frame() |>
    rename(x = V1, y = V2)
}

oldpar <- par()

library(spatstat)

par(mfrow = c(1, 2))
ppp <- spaced_points(1000) |> as.ppp(W = owin())
ppp |> envelope(pcf) |> plot(ylim = c(0, 4))
ppp |> density() |> plot()
ppp |> points(col = "white", cex = 0.35)
