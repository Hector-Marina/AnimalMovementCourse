###############################################################X
#----Analysis of Animal Movement Data in R Workshop------------X
#---------------- Home-Range Estimation -----------------------X
#----------------Last updated 2025-09-07-----------------------X
#---------------------- Exercise ------------------------------X
#--- Prepared by: Johannes Signer (jsigner@gwdg.de) -----------X
#---------------- Bernardo Niebuhr (bernardo.brandao@nina.no) -X
#---------------- based on material by Brian Smith ------------X
###############################################################X

# Load packages ----
library(tidyverse)
library(terra)
library(amt)

# Prepare data ----
# Load prepared tracks 
dat <- read_rds("data/processed/outdoor/gps_data_track.rds")

# Subset to one random individual
a1 <- dat %>% 
  filter(id == "FIT_421931")

# Home range functions ----
# The home range constructor functions in `amt` start with 'hr_*()'.
# E.g., to create an MCP, use 'hr_mcp()'.
# see here: 
?hr_mcp

# MCPs ----
# MCPs are the simplest home range estimator.
# They have only one argument: the level.

# We can calculate multiple MCPs with different levels, by setting the `levels`
# argument. 

mcps <- hr_mcp(a1, levels = c(0.5, 0.75, 0.95, 1))
mcps

# The object `mcps` is a list with following arguments:
#   - `mcp`: simple features (sf) data.frame with a row for each mcp level
#   - `levels`: vector of the levels we wanted
#   - `estimator`: character string giving the type of estimator ("mcp")
#   - `crs`: object of class "crs" giving the coordinate reference system
#   - `data`: data.frame of class "track_xyt" with the original input data

# Results from other home-range estimators within `amt` have a similar structure.
# For a more comprehensive overview see also: https://peerj.com/articles/11031/

# Each 'hr' object created by `amt` also has default "methods" (functions)
#   * 'hr_area()' -- calculates the home range area
#   * 'hr_isopleth()' -- returns the home range isopleths at specified levels
#   * 'hr_overlap()' -- calculates the overlap between two home ranges
#   * 'plot()' -- plots the home range

# These work as follows for MCPs, but also for other estimators

# Area
hr_area(mcps) # returns a tibble
hr_area(mcps, units = TRUE) # returns area as a 'units' object for easy conversion
# Get area in km^2
hr_area(mcps, units = TRUE) %>% 
  mutate(area = units::set_units(area, "km^2"))

# Isopleths
hr_isopleths(mcps) 
# Returns sf object -- useful, e.g., for export to a ESRI Shapefile
?st_write
# you can also plot the polygons
plot(hr_isopleths(mcps)[1])

# Overlap (obviously overlap will be 1)
hr_overlap(mcps, mcps)

# Plot
plot(mcps) # Plots all isopleths and adds data on top

# Custom plot with ggplot2 (thanks to Brian Smith for this code chunk)
# Since we have access to the sf objects for each polygon, we don't have
# to rely on the default plotting method if we don't want to.

hr_isopleths(mcps) %>% 
  # Make level a factor for discrete color scales
  # Can control order and labels here
  mutate(level = factor(level, 
                        levels = c("1", "0.95", "0.75", "0.5"),
                        labels = c("100%", "95%", "75%", "50%"))) %>% 
  ggplot() +
  geom_sf(aes(color = level), 
          fill = NA, linewidth = 2) +
  geom_point(data = mcps$data, aes(x = x_, y = y_),
             color = "gray30", alpha = 0.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_brewer(name = "MCP Level",
                     palette = "Set1") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.box.background = element_rect(colour = "black", linewidth = 0.8))

# ... ... LoCoHs ----
# LoCoHs are created by fitting MCPs to smaller subsets of points, defined
# by a neighborhood rule, which are then merged into larger polygons.
# Unlike MCPs, the resulting polygons are not necessarily convex and
# can also have holes.

# The LoCoH algorithms require more user decisions. Like with all estimators,
# the user must choose the isopleth levels. Users also must choose the
# neighborhood rule ("k", "r", or "a") and the parameter for the neighborhood
# algorithm.

# LoCoHs can approximate a utilization distribution if many isopleths
# are drawn.

# Here, we will use the "k" nearest neighbor algorithm, the simplest choice.
# We will create hulls with the 20 nearest neighbors (k = 21). We'll ask the
# algorithm to fit an isopleth for every 10th percentile.

locohs <- hr_locoh(a1, type = "k", n = 21, levels = seq(0.1, 1, by = 0.1))

# And remember that we have the four methods available
hr_area(locohs, units = TRUE)
hr_isopleths(locohs)
# We can calculate the overlap between MCP and LoCoH home-range estimates
hr_overlap(mcps, locohs)
# plot
plot(locohs)

# It is hard to tell what's going on with the LoCoH without some shading, since
# there are multiple polygons and even holes. Let's make a custom plot.
(locoh_plot <- hr_isopleths(locohs) %>% 
    mutate(level = fct_rev(factor(level))) %>% 
    ggplot() +
    geom_sf(aes(fill = level,
                color = level), 
            linewidth = 1) +
    xlab(NULL) +
    ylab(NULL) +
    scale_color_manual(name = "Isopleth",
                       values = rev(gray.colors(10))) +
    scale_fill_manual(name = "Isopleth",
                      values = rev(gray.colors(10))) +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.box.background = element_rect(colour = "black", linewidth = 0.8)))

# Kernel density estimation in general ----
# The simplest probabilistic estimator is the KDE. It assumes all locations
# are independent, which is probably a bad assumption for frequent GPS fixes.

# Methods currently implemented in `amt` are: 
#   * 'hr_kde_ref()' -- the reference bandwidth (appropriate for unimodal UDs).
#   * 'hr_kde_ref_scaled()' -- a scaled version of the reference bandwidth that
#                             finds the smallest bandwidth that produces a
#                             home range isopleth consisting of n (usually 1)
#                             polygons. Suggested by Kie 2013.
#   * 'hr_kde_pi()' -- the plug-in bandwidth. Suggested by Gitzen et al. 2006.
#   * 'hr_kde_lscv()' -- the least squares cross validation bandwidth. Suggested
#                       by Seaman & Powell 1996.

# We'll demonstrate the reference bandwidth here, which is the default.

# Fit KDEs
kdes <- hr_kde(a1, levels = c(0.5, 0.95), )

# Examine our object
class(kdes)
str(kdes)

# Two important additions are two 'SpatRaster' objects:
#   * 'ud' -- the rasterized utilization distribution
#   * 'trast' -- the template raster used to rasterize the UD

# All 4 methods are available for our probabilistic estimators, too.
hr_area(kdes, units = TRUE)
hr_isopleths(kdes)
# Skipping hr_overlap()
plot(kdes)

# aKDE ----

# The `amt` function 'hr_akde()' provides a very convenient interface
# to the `ctmm` package. aKDEs are fit in `amt` via a wrapper to the `ctmm` 
# functions. Many more details are available here:
?ctmm::akde

# The aKDE relies on a continuous time movement model.
# `amt` also provides a convenient wrapper for fitting those.
?fit_ctmm

# Without going into detail on the different CTMMs, we'll demonstrate fitting
# an aKDE with an Ornstein-Uhlenbeck model.
akdes <- hr_akde(a1, model = fit_ctmm(a1, "ou"), levels = 0.95)

# Examine the object
class(akdes)

# Structure
str(akdes, max.level = 1)

# Notice that the 'ud' and 'trast' elements are still available, as with the KDE
# Notice that we also have the 'akde' and 'model' objects from `ctmm`

# Our methods still apply
hr_area(akdes, units = TRUE) # notice we now have confidence intervals around our estimates
hr_isopleths(akdes)
# Skipping hr_overlap()
plot(akdes) # The 3 lines represent the estimate and the confidence bounds

# We have more plotting options with the individual parts
plot(akdes$akde) # plotting method from `ctmm`

# ... E: Exercise ---- 
# Calculate a MCP100 home range for two individuals ("FIT_421934" and "FIT_421906") 
# and calculate their overlap.

# ... S: Solution -----
i1 <- dat |> filter(id == "FIT_421934")
i2 <- dat |> filter(id == "FIT_421935")

hr1 <- hr_mcp(i1, level = 1)
hr2 <- hr_mcp(i2, level = 1)

hr_overlap(hr1, hr2)



# ... E: Exercise ---- 
# Calculate a MCP home range for all individuals at the 90% isopleth level and
# plot the result for all individuals.

# ... S: Solution -----

mcps <- dat |> nest(data = -id) |> 
  mutate(mcp = map(data, ~ hr_mcp(.x, levels = c(0.9)))) 

mcps_pols <- map(mcps$mcp, ~ hr_isopleths(.)) |> 
  dplyr::bind_rows() |> 
  dplyr::bind_cols(mcps[,1])

plot(mcps_pols[4])
# or
mcps_pols |> 
  ggplot() + 
  geom_sf(aes(color = id, fill = id), alpha = 0.3)


# ... E: Exercise ---- 
# Now calculate the pair-wise overlap between the MCP of the individuals
# (have a look at the documentation of `hr_overlap()`). 
# Can you think of a nice way of plotting the results?

# ... S: Solution -----

mcps <- dat |> nest(data = -id) |> 
  mutate(mcp = map(data, ~ hr_mcp(.x, levels = c(0.9)))) 

res1 <- hr_overlap(mcps$mcp, labels = mcps$id, which = "all")
res1
res1 |> ggplot(aes(x = from, y = to, fill = overlap)) + geom_raster() +
  scale_fill_viridis_c() +
  theme_light() +
  coord_equal() +
  theme(axis.text.x = element_text(angle = 90))

