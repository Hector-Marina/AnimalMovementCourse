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
dat <- read_rds("data/processed/outdoor/gps_data_track.rds")

# !!! This is not necessary here, but remember the GPS data and the environmental data
# must always be on the same CRS 
# change projection of the trajectory to make it fit with the environmental data
# dat <- dat %>% transform_coords(3006)

# Load the environmental data
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

#' First we calculate the home range for a1
#' The home range computations assume independence between positions
#' We did not account for that in the earlier estimations of home range, but now
#' we resample the track to 4 hours to have a regular fix rate

a1 <- track_resample(a1, rate = hours(4), tolerance = minutes(10))
a1

#' Create an kernel home range and convert it to an sf-object
#' to be able to overlay this with the raster data later.
#' You might want to check the hr_kde() function.
?hr_kde 

kde1 <- hr_kde(a1, levels = 0.5)

plot(kde1)
class(kde1)

# now we compute the kernel density estimation 50% isopleth for this
# individual and transform it into a sf-polygon object
kde1_pol <- hr_isopleths(kde1)

# another solution, preparing the terrain for computing the home range
# for all individuals
# using the `hr_to_sf` function
# kde1_pol <- a1 |> nest(data = -id) |>
#   mutate(hr = map(data, hr_kde, levels=0.5), n = map_int(data, nrow)) |>
#   hr_to_sf(hr, id, n) 

#' Now we need to extract the environmental variables within both the individual 
#' home range and the common home range for all individuals.
#' We will use the terra-package to extract values from the raster to the polygons.
#' Here is some more tutorials on terra: https://tmieno2.github.io/R-as-GIS-for-Economists/int-RV.html

#' First the polygon needs to be converted to a SpatVector
kde1_v <- vect(kde1_pol)
str(kde1_v)
plot(kde1_v)

#' Now we can extract the nmd-land use classes for the polygon. This will give you 
#' a data.frame with one row for each cell with the cell center within the polygon. 
#' Each row will also have a "land_use" code, this is the land cover class in NMD
kde1_v_env <- terra::extract(land_cover, kde1_v)  
class(kde1_v_env)
head(kde1_v_env)

#' check the number of classes and observations in each class
table(kde1_v_env$land_cover)

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
    data.resampled = map(data, ~ track_resample(.x, rate = hours(4), tolerance = minutes(10)))) |> 
  dplyr::select(id, data.resampled)

# unnest the data as we want to estimate a home range for all individual at the same time 
dat2 <- dat2 |> 
  unnest(cols = data.resampled)

#' make a common MCP for all individuals this will be used as the availability area 
mcp_all <- hr_mcp(dat2, levels=1)
plot(mcp_all)

# we can take only the sf object (the polygon)
mcp_all_pol <- hr_isopleths(mcp_all)
# or 
# mcp_all$mcp

plot(mcp_all_pol[1])

#' Now we extract the land cover for the population range polygon.
#' First the polygon needs to be converted to a SpatVector
mcp_all_v <- vect(mcp_all_pol)
str(mcp_all_v)
plot(mcp_all_v)

#' Now we can extract the nmd-classes for the common area for all animals. 
#' This might take some time...
mcp_all_v_env <- terra::extract(land_cover, mcp_all_v)  
class(mcp_all_v_env)
head(mcp_all_v_env)

#' check the number of classes and observations in each class
table(mcp_all_v_env$land_cover)

# Plot of both individual and population home ranges
plot(mcp_all_v)
plot(kde1_v, add = TRUE)

#' Now calculate the proportion of each land cover type and from this compute Manly's index
#' Use the prop.table-function to calculate the proportions of land cover in each area

prop_a1 <- as.data.frame(prop.table(table(kde1_v_env$land_cover)))
prop_all <- as.data.frame(prop.table(table(mcp_all_v_env$land_cover)))

#' Merge the two data.frames, note the number of land cover types in each area.
manly <- merge(prop_a1, prop_all, by = c("Var1")) |> 
  rename(code = Var1, prop_a1 = Freq.x, prop_all = Freq.y)

#' We can now retrieve the name of the land cover classes.
manly <- merge(manly, lc_class, by = c("code"))

#' Finally, we compute the Manly index
manly$index <- manly$prop_a1/manly$prop_all

print(manly)

#' How do you interpret these numbers?


# ... E: Exercise ---- 
#' What would change if you compute the proportions for the nominal land cover class
#' instead of the code (e.g. pine forests, spruce forests etc)? 
#' Would the results be consistent?
#' Notice that several of the codes are grouped in the same land cover class.
#' 
#' Tip: you can use merge() or left_join() to join the land cover class names in 
#' the object lc_class with the classes extracted in kde1_v and mcpall_v, and
#' then re-compute the Manly index using these text classes.

# ... S: Solution -----
kde1_v_env <- left_join(kde1_v_env, lc_class, by = join_by(land_cover == code))
mcp_all_v_env <- left_join(mcp_all_v_env, lc_class, by = join_by(land_cover == code))

prop_a1 <- as.data.frame(prop.table(table(kde1_v_env$class)))
prop_all <- as.data.frame(prop.table(table(mcp_all_v_env$class)))

manly_classes <- merge(prop_a1, prop_all, by = c("Var1")) |> 
  rename(code = Var1, prop_a1 = Freq.x, prop_all = Freq.y)
manly_classes$index <- manly_classes$prop_a1/manly_classes$prop_all
print(manly_classes)
# in increasing order of selection
manly_classes |> arrange(index)


# ... E: Exercise ---- 
#' What would change if we used the 50% KDE for the individual a1 (instead of the
#' 50% KDE), and the 95% KDE for this same individual?
#' Why do these differences arise?
#' 
#' You need to recompute the Manly index for this individual using these measures.
#' !!!!!!
 
# ... S: Solution -----

#----
# compute core areas for individual a1
kde1 <- hr_kde(a1, levels = 0.5)
plot(kde1)
kde1_pol <- hr_isopleths(kde1)
# convert polygon toa SpatVector
kde1_v <- vect(kde1_pol)
# extract
kde1_v_env <- terra::extract(land_cover, kde1_v)  
head(kde1_v_env)

#----
# compute whole home range for individual a1
kde1_whole <- hr_kde(a1, levels = 0.99)
plot(kde1_whole)
kde1_whole_pol <- hr_isopleths(kde1_whole)
# convert polygon toa SpatVector
kde1_whole_v <- vect(kde1_whole_pol)
# extract
kde1_whole_v_env <- terra::extract(land_cover, kde1_whole_v)  
head(kde1_whole_v_env)

#--- 
# what we are comparing now
plot(kde1_whole_v)
plot(kde1_v, add = T)

#---
# compute manly's index
# proportions
prop_a1 <- as.data.frame(prop.table(table(kde1_v_env$land_cover)))
prop_whole <- as.data.frame(prop.table(table(kde1_whole_v_env$land_cover)))
# merge
manly_a1 <- merge(prop_a1, prop_whole, by = c("Var1")) |> 
  rename(code = Var1, prop_a1 = Freq.x, prop_all = Freq.y)
# get land cover casses
manly_a1 <- merge(manly_a1, lc_class, by = c("code"))
# compute the Manly index
manly_a1$index <- manly_a1$prop_a1/manly_a1$prop_all
print(manly_a1)
# compare
manly_a1$index_kde50_all <- manly$index
print(manly_a1)

# ---------------


#' # RSF one individual ------
#' Test to create available random points from the track within a MCP.
#' using the random_points()-function and the track object `a1` as input
r1 <- random_points(a1) 
plot(r1)
str(r1)

#' We can also create available random points within a predefined home range
#' For instance, we try with a kernel density estimation.
kde1 <- hr_kde(a1, levels = c(0.95))
plot(kde1)

#' Create the random points again, now within the polygon of the kde.
#' To add the observed locations to the data, we use the `presence` argument.
r2 <- random_points(kde1, presence = a1)
plot(r2)

#' When using the hr instead of the track object as the main input, we can 
#' also add the number of random points using the parameter `n`.
#' Here we create 10 random points for each observed location.
r3 <- random_points(kde1, n=nrow(a1) * 10, presence = a1) 
plot(r3)

# Note the difference in number of random points between r2 and r3.
table(r2$case_)
table(r3$case_)

#' As in the Manly's index we need to annotate the data, i.e., 
#' extract the covariates and add their information to each position, both 
#' random and observed. 
#' Now we can use the function within amt `extract_covariates()`. We could also use
#' the `extract()` function from the `terra` package as done above for the Manly
#' index exercise.
#' We use the r3 data where we have 10 times the available 
#' locations compared to observed locations. 

r3_annotated <- r3 |> 
  # here we extract the land cover classes
  extract_covariates(land_cover) |>
  # and then we change names of the land cover classes 
  # !!!!!!!!!!!!!!!!!
  dplyr::mutate(landcover = factor(land_cover,
                                   levels = lc_class$code,
                                   labels = lc_class$class) |> 
                  relevel(ref = "pine forests")) |>
  # then the elevation layer is extracted and we change the name
  extract_covariates(dem) |>
  # clean up a bit and select only the columns needed 
  dplyr::select(case_, x_, y_, landcover, dem)

r3_annotated

#' Now we can fit a RSF. We will use `fit_rsf()` function from the `amt`-package, 
#' which is just a wrapper around stats::glm with family = binomial(link = "logit").

r3_annotated |> fit_rsf(case_ ~ landcover + dem) |> 
  summary()

# ... E: Exercise ---- 
#' Try to interpret the results of the RSF above. How should the estimates of land 
#' cover classes be interpreted?
#' 
#' Tip: When we have categorical variables among the environmental covariates,
#' one of the categories is set as the reference (here the intercept in our fit), 
#' and the selection coefficients are comparisons to the selection strength in
#' this reference category.
#' In these cases, it is generally useful to add a "-1" to the formula, so this
#' reference level is explicitly stated. 
rsf1 <- r3_annotated |> 
  fit_rsf(case_ ~ landcover + dem - 1)
summary(rsf1)




#----------------------


# ... E: Exercise 1---- 
#' We can also generate random points for another predefined area. This can 
#' be useful if you want to estimate the habitat selection at second-order (Johnson 1980).
#' Use the common MCP we produced above for all animals in the data set 
#' and create random points within that area. Then fit a 2nd-order RSF.

# ... S: Solution 1-----

# availability area
dat2 <- dat |> 
  nest(data = -id) |> 
  mutate(
    data.resampled = map(data, ~ track_resample(.x, rate = hours(4), tolerance = minutes(10)))) |> 
  dplyr::select(id, data.resampled)

# unnest the data as we want to estimate a home range for all individual at the same time 
dat2 <- dat2 |> 
  unnest(cols = data.resampled)

# make a common MCP for all individuals this will be used as the availability area
# skip definition of the id and you'll get a common MCP
mcp_all <- hr_mcp(dat2, levels=1)

# create random points within the common mcp
r_sec <- random_points(mcp_all, n=nrow(a1)*10, presence = a1) 
plot(r_sec)
r_sec

# annotate the data
r_sec_annotated <- r_sec |> 
  # here we extract the land cover classes
  extract_covariates(land_cover) |>
  # remove water
  filter(land_cover != 61) |> 
  # and then we change names of the land cover classes 
  dplyr::mutate(landcover = factor(land_cover,
                                   levels = lc_class$code,
                                   labels = lc_class$class) |> 
                  relevel(ref = "pine forests") |> 
                  droplevels()) |>
  # then the elevation layer is extracted and we change the name
  extract_covariates(dem) |>  
  # clean up a bit and select only the columns needed 
  dplyr::select(case_, x_, y_, landcover, dem)

r_sec_annotated$landcover

manly_classes

rsf2 <- r_sec_annotated |> fit_rsf(case_ ~ landcover + dem)
summary(rsf2)

# if the availability of pine forests and open wetlands was the same,
# the relative selection of open wetlands would be
exp(coef(rsf2)[2])*manly_classes$prop_all[manly_classes$code == "open wetlands"]/manly_classes$prop_all[manly_classes$code == "pine forests"]

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






