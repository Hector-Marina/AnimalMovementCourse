#'###################################################################X
#----Analysis of Animal Movement Data in R Workshop------------------X
#-------------------- Habitat selection  ----------------------------X
#----------------Last updated 2023-08-24-----------------------------X
#---------------------- Exercise  -----------------------------------X
#-Prepared by: Anna Skarin (anna.skarin@slu.se)  --------------------X
# ------------ Bernardo Niebuhr (bernardo.brandao@nina.no) ----------X
#-inspired from the vignette: ---------------------------------------X
#-https://cran.r-project.org/web/packages/amt/vignettes/p3_rsf.html -X
#####################################################################X

rm(list=ls())
# Load packages ----
library(tidyverse)
library(amt)
library(sf)
library(terra)
library(lubridate) # to deal with date and time
library(ggplot2)
#------------------

#' We first start with computing Manly's resource index for one animal where 
#' the proportions of habitat used is related to the habitat available to the animal. 
#' First, we need to define the habitat available for the animal. 
#' This can be done in several ways and at different geographical scales. 
#' We can use the GPS locations and compute a home range of that animal 
#' for the given time period, or we can define the whole region that the whole 
#' population/group of animal is using based on both GPS-data and expert knowledge.

# Load data for the exercises
# Load prepared tracks 
dat <- read_rds("data/processed/outdoor/dat_track3035.rds")

# change projection of the trajectory to make it fit with the environmental data
dat <- dat %>% transform_coords(3006)

# load the environmental data
# Load the Swedish national land cover data (NMD)
land_cover <- terra::rast("data/raw/outdoor/nmd10_ungeneralized_stakke.tif")
names(land_cover) <- "land_cover"
land_cover

# Load elevation data (DEM)
dem <- terra::rast("data/raw/outdoor/DEM10_stakke.tif")
names(dem) <- "dem"
dem

# plot
plot(land_cover)
plot(dem)

# read classes for the land cover map
lc_class <- read.csv2("data/raw/outdoor/nmd_classes_eng_reclassified_course.csv", header = F,
                     col.names = c("code", "class"), stringsAsFactors = F) |>
  dplyr::arrange(code)


# MANLY'S INDEX ----

# Subset to one individual
a1 <- dat %>% 
  filter(id == "FIT_421931")

# First we calculate the home range for a1
# The home range computations assume independence between positions
# We did not account for that in the earlier estimations of home range, but now
# we resample the track to 4 hours to have a regular fix rate

a1 <- track_resample(a1, rate = hours(4), tolerance = minutes(10))
a1

#' Create an kernel home range which is converted to an sf-object
#' to be able to overlay this with the raster data later. 

kde1 <- hr_kde(a1, levels = 0.5)

plot(kde1)
class(kde1)

kde1 <- a1 |> nest(data = -id) |>
  mutate(hr = map(data, hr_kde, levels=0.5), n = map_int(data, nrow)) |>
  hr_to_sf(hr, id, n) 


#' Now we need to extract the environmental variables within both the individual 
#' home range and the common home range for all individuals.
#' We will use the terra-package to extract values from the raster to the polygons.
#' Here is some more tutorials on terra: https://tmieno2.github.io/R-as-GIS-for-Economists/int-RV.html

#' First the polygon needs to be converted to a SpatVector
kde1_v <- vect(kde1)
str(kde1_v)
plot(kde1_v)

#' Now we can extract the nmd-classes for the polygon. This will give you a data.frame with
#' one row for each cell with the cell center within the polygon. Each row will also have a 
#' "Layer_1" this is the land cover class in NMD
kde1_v <- terra::extract(land_cover, kde1_v)  
class(kde1_v)
head(kde1_v)

#' check the number of classes and observations in each class
table(kde1_v$land_cover)

#' As we know the size of the cells in the raster we can calculate the size of each 
#' land cover type if we want. However it is enough to just use the number of observations 
#' to calculate the percentage of each land cover in the home range.


#' To compare the used area of individual FIT_421931 with how the rest of 
#' the population use the area we can create a common MCP for all individuals.
#' Use the solution from the first exercise and resample all data 
#' to 4 hour fix rate to have the same fix rate for all individuals. 

dat2 <- dat |> 
  nest(data = -id) |> 
  mutate(
    data.resampled = map(data, ~ track_resample(.x, rate = hours(4), tolerance = minutes(10))), 
    steps = map(data.resampled, steps_by_burst)) |> 
  dplyr::select(id, steps)

# unnest the data as we want to estimate a home range for all individual at the same time 
dat2 <- dat2 |> 
  unnest(cols = steps)

# make a track of the data to be able to use it for making home range
dat2 <- make_track(dat2,.x=x1_,  .y=y1_,  .t=t1_ , id=id)
plot(dat2)

# # make a common MCP for all individuals this will be used as the availability area 
# mcpall <- hr_mcp(dat2, levels=1)
# 
# Here we want to convert the mcp to and sf object
# but it does not work!!!
mcpall <- hr_to_sf(mcpall, ...)


#instead i made like this from the help of hr_to_sf-function
# this works but i get kind of the wrong MCP
# but maybe its ok??
 
mcpall2 <- dat2 |> nest(data = -id) |>
  mutate(hr = map(data, hr_mcp, levels=1), n = map_int(data, nrow)) |>
  hr_to_sf(hr, id, n) |>
  st_union()

plot(mcpall2)

# First the polygon needs to be converted to a SpatVector
mcpall_v <- vect(mcpall2)
str(mcpall_v)

#' Now we can extract the nmd-classes for the common area for all animals. 
mcpall_v <- terra::extract(nmd, mcpall_v)  
class(mcpall_v)
head(mcpall_v)

#' check the number of classes and observations in each class
table(mcpall_v$Layer_1)

#' Now calculate the proportion of each land cover type and from this compute Manly's index
#' Use the prop.table-function to calculate the proportions of land cover in each area

prop_a1 <- as.data.frame(prop.table(table(kde1_v$Layer_1)))
prop_all <- as.data.frame(prop.table(table(mcpall_v$Layer_1)))

# Merge the two data.frames, note the number of land cover types in each area.
manly <- merge(prop_a1, prop_all, by = c("Var1"))
manly$index <- manly$Freq.x/manly$Freq.y

print(manly)

# ---------------


# RSF one individual ------
# Test to create available random points from the track within a MCP.
# using the random_points - function
r1 <- random_points(a1) 
plot(r1)
str(r1)

# We can also create available random points within a predefined home range
# now we try with a kernel density estimation.
kde1 <- hr_kde(a1, levels = c(0.95))
plot(kde1)

# Create the random points, to add the observed locations 
# to the data use the presence-argument.
r2 <- random_points(kde1, presence = a1)
plot(r2)

# Using the hr instead of the track object we need to add the observed points
# using the presence-argument.
# The number of random points can be defined using n=...
r3 <- random_points(kde1, n=nrow(a1)*10, presence = a1) 
plot(r3)

# Note the difference in number of random points between r2 and r3.
table(r2$case_)
table(r3$case_)

#' As in the Manly's index we need to extract the covariates and 
#' ad their information to each position, both random and observed. 
#' Now we can use the function within amt extract_covariates. However, 
#' for this function we need the data to be in raster-format. 
#' We use the r3 data where we have 10 times the available 
#' locations compared to observed locations. 

nmd <-raster(nmd)
dem <- raster(dem)

#check out the layers
class(dem)
plot(dem)

class(nmd)
plot(nmd)

rsf1 <- r3 |> 
  # here we extract the land cover classes
  extract_covariates(nmd) |>
  # and then we change names of the land cover classes 
  dplyr::mutate(landcover = factor(Layer_1,
                                   levels = lc_class$code,
                                   labels = lc_class$class)) |>
  # then the elevation layer is extracted and we change the name
  extract_covariates(dem) |>
  dplyr::mutate(dem = DEM10_stakke) |>
  # clean up a bit and select only the columns needed 
  dplyr::select(case_, x_, y_, landcover, dem)

rsf1
#' Now we can fit a RSF. We will use fit_rsf function from amt, 
#' which is just a wrapper around stats::glm with family = binomial(link = "logit").

rsf1 |> fit_rsf(case_ ~ landcover + dem) |> 
  summary()

# Try to interpret the results, how should the estimates of land cover classes be interpreted?
#----------------------


# ... E: Exercise 1---- 
#' We can also generate random points for another predefined area. This can 
#' be useful if you want to estimate the habitat selection at second-order (Johnson 1980).
#' Use the common MCP we produced above for all animals in the data set 
#' and create random points within that area.







# ... S: Solution 1-----
# availability area
dat2 <- dat |> 
  nest(data = -id) |> 
  mutate(
    data.resampled = map(data, ~ track_resample(.x, rate = hours(4), tolerance = minutes(10))), 
    steps = map(data.resampled, steps_by_burst)) |> 
  dplyr::select(id, steps)

# unnest the data as we want to estimate a home range for all individual at the same time 
dat2 <- dat2 |> 
  unnest(cols = steps)

# make a track of the data to be able to use it for making home range
dat2 <- make_track(dat2,.x=x1_,  .y=y1_,  .t=t1_ , id=id)
plot(dat2)

# make a common MCP for all individuals this will be used as the availability area
# skip definition of the id and you'll get a common MCP
mcpall <- hr_mcp(dat2, levels=1)

# create random points within the common mcp
r_sec <- random_points(mcpall, n=nrow(a1)*10, presence = a1) 
plot(r_sec)
r_sec

#need to convert to raster format
nmd <-raster(nmd)
dem <- raster(dem)

rsf2 <- r_sec |> 
  # here we extract the land cover classes
  extract_covariates(nmd) |>
  # and then we change names of the land cover classes 
  dplyr::mutate(landcover = factor(Layer_1,
                                   levels = lc_class$code,
                                   labels = lc_class$class)) |>
  # then the elevation layer is extracted and we change the name
  extract_covariates(dem) |>
  dplyr::mutate(dem = DEM10_stakke) |>
  # clean up a bit and select only the columns needed 
  dplyr::select(case_, x_, y_, landcover, dem)

rsf2

rsf2 |> fit_rsf(case_ ~ landcover + dem) |> 
  summary()


# ... E: Exercise 2---- 
#' Use the same animal as above in the exercise "RSF one individual" 
#' and fit and RSF at second-order, i.e. placement of home range 
#' within the available area for all individuals. 
#' 
#' Compare the estimates for the second-order and third-order model, 
#' is there any difference? Why?






# ... S: Solution 2-----


#-------



# used and available
used_ava <- dplyr::bind_rows(used, ava)

used_ava_sf <- used_ava %>% 
  sf::st_as_sf(coords = c("x_", "y_")) %>% 
  terra::vect()

used_ava %>% 
  sf::st_as_sf(coords = c("x_", "y_")) %>% 
  ggplot() +
  geom_sf(aes(colour = case_))

background_data <- terra::extract(maps, used_ava_sf) %>% 
  as.data.frame()






