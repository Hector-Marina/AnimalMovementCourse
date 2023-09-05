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
library(tidyverse)
library(amt)
library(sf)
library(terra)
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
lc_class <- read_csv2("data/raw/outdoor/nmd_classes_eng_reclassified_course.csv", 
                      col_names = F) |> 
  rename(ID = 1, class = 2) |> 
  dplyr::arrange(code)
lc_class

#' Now we can reclass the land cover codes, and plot it in a more meaningful way:
land_cover <- as.factor(land_cover)
levels(land_cover) <- lc_class
# if you get an error, maybe you need this:
# levels(land_cover)[[1]] <- data.frame(lc_class)

plot(land_cover, main = "Land cover")
 
#------------------
#' Load movement data
#' 
#' Now we load the movement data again. As usual, and as a good practice, we
#' start with one individual, resample the data to 4h, and compute steps. 

# Load data for the exercises
# Load prepared tracks 
dat <- read_rds("data/processed/outdoor/gps_data_track.rds")

# Subset to one individual
a1 <- dat %>% 
  filter(id == "FIT_421931")

# resample, compute steps
a1 <- track_resample(a1, rate = hours(4), tolerance = minutes(10)) |> 
  steps_by_burst()

#------------------
#' Annotate data
#' 
#' We call "data annotation" to the process of crossing the movement data with
#' background information related to different spatial or non-spatial variables.
#' To annotate the data with spatial data, we can use the `extract()` function of
#' the `terra` package. We can also use the function `extract_covariates()` from the
#' amt-package, which is a wrapper around `terra::extract()`.

#' We start with the terra package.

# terra - extract values at the start position of a step
terra::extract(land_cover, data.frame(a1[c("x1_", "y1_")]))

#' We see the result is a data.frame! This needs to be attached as a new column in 
#' the movement track tibble object.
#' 
#' For comparison, we use the amt - extract_covariates function.
a1_env <- extract_covariates(a1, land_cover, where = "start")

#' Notice that now the structure of tibble with computed steps is still kept, and
#' the land cover was added as a new column already. Also, we defined 
#' `where = "start"` because we are interested in the land cover in the points
#' at the start of a step.
a1_env

# ... E: Exercise ---- 
#' Make a bar plot showing the number (or proportion) of positions in each land
#' cover class. This tells us a little about the animal space/habitat use.
#' 
#' hint: you can use `geom_bar()` from `ggplot2` package. 

# ... S: Solution -----





#------------------

# ... E: Exercise ---- 
#' Repeat the procedure above for the elevation dataset and plot the number
#' of position along the elevation gradient the animal used.
#' 
#' hint you can make a `geom_histogram` from `ggplot2` package.

# ... S: Solution -----






#------------------
#' Step lengths vs environmental covariates
#' 
#' Now we'll use data from all individuals and will annotate the data set with
#' the covariates. 
#' 
#' As usual, we first nest the data.frame, resample to 4h, and compute the steps
#' per burst, and finally unnest to have a tibble structure again.

dat2 <- dat |> 
  # nest by id
  nest(data = -id) |>
  # create new list column by resampling the data and computing steps
  # this creates a new column with resampled track objects with step properties
  mutate(
    data.resampled.steps = map(data, ~ .x |> 
                                 track_resample(rate = hours(4), tolerance = minutes(10)) |>
                                 steps_by_burst())) |> 
  # select only id and resampled.step columns
  # (we do not need the original data.frame anymore)
  dplyr::select(id, data.resampled.steps) |> 
  # unnest 
  unnest(cols = data.resampled.steps)

# extract environmental information at the starting location of each step
dat2_env <- dat2 |> 
  amt::extract_covariates(land_cover, where = "start") |> 
  amt::extract_covariates(dem, where = "start")

#' We can now plot how the step varies with land cover class.

dat2_env |> 
  ggplot(aes(x = class, y = sl_)) +
  geom_boxplot() +
  coord_flip()

#' We can look closer at values of step length until maximum 5000 m.
#' 
dat2_env |> 
  ggplot(aes(x = class, y = sl_)) +
  geom_boxplot() +
  ylim(0, 5000) +
  coord_flip()

#' We can also do the same for the turning angles now.
dat2_env |> 
  ggplot(aes(x = ta_)) +
  geom_histogram() +
  facet_wrap(~class)

#' In which land cover class are the trajectories more tortuous/slow and in 
#' which they are more linear/directional?






#-------------

# ... E: Exercise ---- 
#' Explore how the step lengths vary with elevation.

# ... S: Solution -----







# ... E: Exercise ---- 
#' Compute the time of the day using the `time_of_day()` function of the 
#' `amt`-package. Is there a difference in the proportion of positions per day
#' in different land cover types between day and night?
#' 


# ... S: Solution -----








# ... E: Exercise ---- 
#' Using the same dataset, plot and explore how the step lengths change
#' between day and night for different land cover classes


# ... S: Solution -----

