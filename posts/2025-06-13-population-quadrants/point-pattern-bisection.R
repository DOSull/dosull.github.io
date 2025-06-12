library(ggplot2)
library(dplyr)
library(tidyr)
library(R.utils)

n <- 1000
points <- data.frame(
  x = runif(n, -1, 1),
  y = runif(n, -1, 1),
  n = 1
  # n = 2 * rpois(500, 3) + 3
)

bisector <- function(pts, angle) {
  a <- angle * pi / 180
  rot_m <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2, byrow = 2)
  pts_r <- rot_m %*% t(pts[, 1:2])
  median_x <- median(pts_r[1, ])
  list(A = sin(a), B = -cos(a), C = -median_x * sin(a))
}

xintercepts <- c()
yintercepts <- c()
slopes <- c()
intercepts <- c()

for (angle in seq(0, 175, 5)) {
  bs <- bisector(points, angle)
  if (isZero(bs$A)) {
    yintercepts <- c(yintercepts, bs$C / bs$B)
  } else if (isZero(bs$B)) {
    xintercepts <- c(xintercepts, bs$C / bs$A)
  } else {
    slopes <- c(slopes, -bs$A / bs$B)
    intercepts <- c(intercepts, -bs$C / bs$B)
  }
}

ggplot(points) + 
  geom_point(aes(x = x, y = y), size = 1, shape = 1) +
  coord_equal(xlim = .25 * c(-1, 1), ylim = .25 * c(-1, 1)) + 
  geom_hline(
    data = data.frame(y = yintercepts), 
    aes(yintercept = y), lwd = 0.35, colour = "red", linetype = "dashed") +
  geom_vline(
    data = data.frame(x = xintercepts), 
    aes(xintercept = x), lwd = 0.35, colour = "red", linetype = "dashed") +
  geom_abline(
    data = data.frame(slope = slopes, intercept = intercepts), 
    aes(slope = slope, intercept = intercept), lwd = 0.35, 
    colour = "red", linetype = "dashed") +
  theme_minimal()
  
