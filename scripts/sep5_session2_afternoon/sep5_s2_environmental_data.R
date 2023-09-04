#'###################################################################X
#-----------Analysis of Animal Movement Data in R Workshop-----------X
#----- Annotating movement data with environmental information  -----X
#----------------Last updated 2023-08-24-----------------------------X
#---------------------- Exercise  -----------------------------------X
#-Prepared by: Bernardo Niebuhr (bernardo.brandao@nina.no) ----------X
# ------------ Anna Skarin (anna.skarin@slu.se)  --------------------X
#--------------------------------------------------------------------X
#####################################################################X


# Load packages ----
library(amt)
library(sf)
library(terra)
library(lubridate) # to deal with date and time
library(ggplot2)
#------------------

#------------------
#' # Download and put the raster files in the correct place
#' 
#' First of all, make sure you download the maps representing the environmental
#' variables (NMD, a land cover map; and DEM, a digital elevation model). They are
#' *.tif files and should be put under the folder "data/raw/outdoor/" in our 
#' course repository structure.

#------------------
#' # Load and explore the maps
#' 
#' Now let's start by loading the maps. We'll use the function `rast()` of the 
#' `terra` package.

# Load the environmental data
# Load the Swedish national land cover data (NMD)
land_cover <- terra::rast("data/raw/outdoor/nmd10_ungeneralized_stakke.tif")
land_cover

#' The layer name is not recognized from the *.tif file, so we set it here again
#' so this object has a layer called "land_cover"
names(land_cover) <- "land_cover"
land_cover

# Load elevation data (DEM)
dem <- terra::rast("data/raw/outdoor/DEM10_stakke.tif")
# rename layer
names(dem) <- "dem"
dem

#' Now we can plot these maps to start making sense of them.

# plot
plot(land_cover, main = "Land cover") # different colors are different land cover classes.
plot(dem, main = "Elevation") # elevation

#' The land cover map is categorical, and each class is given a code in the raster
#' file. We now can also read a table from a *.csv file with the class names,
#' so they are more meaningful. 

# read classes for the land cover map
lc_class <- read.csv2("data/raw/outdoor/nmd_classes_eng_reclassified_course.csv", header = F,
                      col.names = c("code", "class"), stringsAsFactors = F) |>
  dplyr::arrange(code)
lc_class

terra::classify()

#------------------

# Load data for the exercises
# Load prepared tracks 
dat <- read_rds("data/processed/outdoor/gps_data_track.rds")



#------------------



#------------------



#------------------
Explore the maps
extract()
proportion of points in each class
explore stel length vs class and vs elevation
extract along and explore sl vs along 