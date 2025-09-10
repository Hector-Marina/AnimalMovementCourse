#######################################################X
#-------------------- Movement Workshop ---------------X
#--------------- Last updated 2025-09-08 --------------X
#------------------ Code Walkthrough ------------------X
#----- Johannes Signer (jsigner@uni-goettingen.de) ----X
#######################################################X

library(lubridate)
library(tidyverse)
library(amt)
library(terra)
library(sf)
set.seed(123)


# Simulate from scratch
# ... Landscape ------------------------------------------------------------

r <- terra::rast(xmin = -100, xmax = 100,
                 ymin = -100, ymax = 100, res = 1)
r[] <- 0
r[80:100, ] <- 1
names(r) <- "x"
plot(r)
r


# ... Redistribution kernel ---------------------------------------------------

start <- make_start(c(0, 0), ta_ = pi/2)
m <- make_issf_model(

  # This is the selection function
  coefs = c(x_end = 2),

  # This is the movement kernel
  sl = make_gamma_distr(shape = 2, scale = 2),
  ta = make_vonmises_distr(kappa = 1))

rdk.1a <- redistribution_kernel(
  m, start = start, map = r,
  landscape = "discrete", as.rast = TRUE, max.dist = 5)
plot(rdk.1a$redistribution.kernel)

rdk.1a <- redistribution_kernel(
  m, start = start, map = r,
  max.dist = 5,
  n.control = 1e4)


# Now simulate one path (i.e. one animal)
p1 <- simulate_path(rdk.1a, n.steps = 20, start = start)
plot(r)
lines(p1$x_, p1$y_)

# Repeat this for 50 animals
n <- 50
system.time(p1 <- replicate(n, simulate_path(rdk.1a, n = 25,
                                             start = start),
                            simplify = FALSE))

# Plot the trajectories
tibble(
  rep = 1:n,
  path = p1
) |> unnest(cols = path) |>
  ggplot(aes(x_, y_, group = rep)) + geom_path(alpha = 0.1) +
  coord_equal() +
  xlim(-100, 100) +
  ylim(-100, 100)

# Smooth output at different points in time
trks <- lapply(c(2, 5, 10, 15, 25), function(i) {
  tibble(
    rep = 1:n,
    path = map(p1, ~ dplyr::slice(.x, i))
  ) |> unnest(cols = path) |>
    make_track(x_, y_) |> hr_kde(trast = r)
})
plts <- terra::rast(lapply(trks, hr_ud))
names(plts) <- paste("n =", c("2", "5", "10", "15", "25"))
terra::plot(plts)

# Contrast this with unconstrained movement
m <- make_issf_model(
  coefs = c(x_end = 0), # No more habitat selection
  sl = make_gamma_distr(shape = 2, scale = 2),
  ta = make_vonmises_distr(kappa = 1))

rdk.1a <- redistribution_kernel(
  m, start = start, map = r,
  landscape = "discrete", as.rast = TRUE, max.dist = 5)
plot(rdk.1a$redistribution.kernel)

rdk.1a <- redistribution_kernel(
  m, start = start, map = r,
  max.dist = 5,
  n.control = 1e4)

# Now simulate one path (i.e. one animal)
p1 <- simulate_path(rdk.1a, n.steps = 20, start = start)
plot(r)
lines(p1$x_, p1$y_)

# Repeat this for 50 animals
n <- 50
system.time(p1 <- replicate(n, simulate_path(rdk.1a, n = 15,
                                             start = start),
                            simplify = FALSE))

# Plot the trajectories
tibble(
  rep = 1:n,
  path = p1
) |> unnest(cols = path) |>
  ggplot(aes(x_, y_, group = rep)) + geom_path(alpha = 0.1) +
  coord_equal() +
  xlim(-100, 100) +
  ylim(-100, 100)

# Smooth output at different points in time
trks <- lapply(c(2, 5, 10, 15), function(i) {
  tibble(
    rep = 1:n,
    path = map(p1, ~ dplyr::slice(.x, i))
  ) |> unnest(cols = path) |>
    make_track(x_, y_) |> hr_kde(trast = r)
})
plts1 <- terra::rast(lapply(trks, hr_ud))
names(plts1) <- paste("n =", c("2", "5", "10", "15"))

terra::plot(plts1)
terra::plot(plts) # constrained movement as comparison from before‚

# ... E (exercise)

# Use the following landscape
set.seed(1243)

r <- terra::rast(xmin = -200, xmax = 200,
                 ymin = -200, ymax = 200, res = 1)

p <- data.frame(x = c(-50, -50, 50, 50), y = c(-50, 50, -50, 50))
p <- st_as_sf(p, coords = c("x", "y"))
p <- st_buffer(p, dist = 40)

plot(p)
r <- rasterize(p, r, background = 0)
names(r) <- "x"

plot(r)


# Can you write a simulation model, where the animals forage in patches (no correlation in turn angles) and travel fast between patches (high correlation between turn angles).

# ... S (solution)




m <- make_issf_model(
  coefs = c(x_end = 1, "x_start:cos(ta_)" = -4), # No more habitat selection
  sl = make_exp_distr(rate = 0.5),
  ta = make_vonmises_distr(kappa = 4))

rdk.1a <- redistribution_kernel(
  m, start = start, map = r,
  max.dist = 5, tolerance.outside = 0.5,
  n.control = 1e4)

# Now simulate one path (i.e. one animal)
p1 <- simulate_path(rdk.1a, n.steps = 1000, start = start)
plot(r)
lines(p1$x_, p1$y_, col = "white")

# Check that we get what we wanted
make_track(p1, x_, y_, t_) |> steps() |>
  extract_covariates(r, where = "both") |>
  ggplot(aes(ta_, fill = factor(x_start))) +
  geom_density(alpha = 0.4) +
  theme_light()
