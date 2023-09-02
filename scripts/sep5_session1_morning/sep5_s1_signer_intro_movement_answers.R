#######################################################X
#----Analysis of Animal Movement Data in R Workshop----X
#-------------------- Introduction --------------------X
#----------------Last updated 2023-08-14---------------X
#---------------------- Exercise ----------------------X
#--- Prepared by: Johannes Signer (jsigner@gwdg.de) ---X
#######################################################X


# General Instruction -----
# The idea is that you work through these exercises on your own or in small 
# groups with help from us. 
# There are extensive instructions to help you with the exercises. In between
# exercises are placed with solutions provided below. Do try first to solve the 
# exercise on your own.

# Load packages ----
library(tidyverse)
library(amt)
library(sf)
library(lubridate) # to deal with date and time
library(ggplot2)

# Load data ---- 

# We use GPS data from reindeer from Sweden. The data is saved as a *.gpkg 
# (GeoPackage), a common GIS vector data format. We can use the function 
# `st_read` from the `sf`-Package to read the into R. 

dat <- st_read("data/processed/outdoor/gpsdata_april_june2021.gpkg")
head(dat)
str(dat)
nrow(dat)
st_crs(dat)
dat

# We now have to make sure that the date column is correctly parsed. 
class(dat$timestamp)

# Currently it is a character, but we want it to be a `POSIXct` 
# (a date-time format for R)

# We can use the function `ymd_hms()` from the `lubridate` package. You could
# also use the `strptime()` function from base R, but `lubirdate`'s parsing
# functions are often a bit easier to memorize.

dat$ts <- ymd_hms(dat$timestamp, tz = "UTC")
head(dat$ts)
str(dat$ts)

# Important: if your time zone is different from UTC (the default), make sure
# you correctly specify the time zone with the argument `tz`.


# Create tracks ----

# The basic building block to work with the `amt` package are the so called
# tracks. A track consists of a series of relocations. The function
# `make_track()` is used to create a track from a `data.frame` or `tibble`. It
# expects at least the data set and the coordinates (x and y). Optionally, time
# stamps, additional columns and a coordinate reference system (crs) can be
# passed to the function using the EPSG code.

# This is a bit trickier here (because we start with a gpkg), we have to start
# by creating first a new data set and then use the function `make_track()`.

dat1 <- data.frame(
  x = st_coordinates(dat)[, 1], 
  y = st_coordinates(dat)[, 2], 
  t = dat$ts, 
  id = dat$Collar_ID
)

head(dat1)

# We can also query the CRS from `dat`.

st_crs(dat)

# Which we can use directly later. 

trk1 <- make_track(dat1, x, y, t, id = id, crs = st_crs(dat))
# or you could set the CRS through the EPSG code:
# make_track(dat1, x, y, t, id = id, crs = 3006)

# Alternatively, if you have the newest version of `amt` you can use the
# function `as_track()` on `dat`.
# Check ?as_track.

# TODO!


# Working with tracks ----

# ... `dplyr` verbs ----

# Tracks are by design compatible with `dplyr`s verbs. For example, if we want
# to work with only one animal, we can just use the `filter()` function.

unique(trk1$id)

animal1 <- trk1 |> filter(id == "FIT_421936")

class(trk1)
class(animal1)

# Other dplyr functions such as mutate, select, arrange, group_by, count,
# summarize work as well.

# ... E: Exercise ---- 
# Use the `mutate()` function to add n new column to
# `animal1` that gives the month when the position was tracked.


# ... S: Solution -----
animal1 |> mutate(month = month(t_)) 


# ... Changing the CRS ----

# We can change the the CRS with the function `transform_coords()`. For example
# to change to geographic coordinates, we could just use:

animal1 %>% transform_coords(4326)

# ... Visually inspect a track -----

# Function `inspect()` allows us to visually check a track with an interactive
# map.

animal1 %>% inspect()


# ... Sampling rate ----

# The rate at which data are sampled for tracks can be different and irregular.
# To get an overview of the sampling rate the function
# `summarize_sampling_rate()` exists.

summarize_sampling_rate(animal1)

# This suggests that the median sampling rate is 2h, but varying up to
# 12h. We can now resample the whole track to 2h interval (with tolerance of
# 10 min), so that if there are more than 2h between relocations, they will 
# be divided into different bursts.

animal2 <- track_resample(animal1, rate = hours(2), tolerance = minutes(10))

# Note the animal was on a different schedule at the start of the study
animal1$t_[1:20]

# And then switched to a 2h interval
animal1$t_[110:140]

# In the resampled track data set, the first positions got separated into
# different bursts.
animal2

# ... E: Exercise ----
# Resample the animal on a 4h sampling rate, with a tolerance of 5 minutes and 
# save the result to `animal3`. 


# ... S: Solution -----
animal3 <- track_resample(animal1, rate = hours(4), tolerance = minutes(5))
animal3


# ... Movement attributes ----

# We can now calculate for example step lengths with the function
# `step_length()`

animal3 %>% step_lengths()

# If want to add step lengths to the data set we can use mutate()

animal4 <- animal3 %>% mutate(sl = step_lengths(.)) 
# note the use of the `.` here, indicates, that we want to refer to the dataset
# that is currently under evaluation.

# We are ignoring bursts
animal4 %>% group_by(burst_) %>% 
  summarize(fs = head(sl, 1), ls = tail(sl, 1)) %>% 
  pivot_longer(-burst_) %>% 
  ggplot(aes(name, value, group = burst_)) + geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.1)


# Let's repeat the same analysis, but this time with bursts now for calculating
# step lengths (we need to nest our data here)
animal5 <- animal3 %>% nest(data = -burst_) %>% 
  mutate(data = map(data, ~ mutate(.x, sl = step_lengths(.x)))) %>% 
  unnest(cols = data)

animal5 %>% group_by(burst_) %>% 
  summarize(fs = head(sl, 1), ls = tail(sl, 2)[1]) %>% 
  pivot_longer(-burst_) %>% 
  ggplot(aes(name, value, group = burst_)) + geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.1)


# ... Steps ---- 
# Next we want to change representations from individual
# locations to steps. A step consists of a start and end coordinate, a step
# length and a turn angle. The time difference between the start and the end
# point is constant.

animal3 %>% steps() 

# We get a warning, because the function `steps()` be default ignores bursts.
# This is problematic if there is a large time gap between to consecutive
# points. To overcome this, we can use `steps_by_burst()`.

s2 <- animal3 %>% steps_by_burst() 

# The resulting tibble has 11 columns by default: 
# - `burst_`: the burst number.
# - `x1_` and `y1_`: the start coordinates of the step.
# - `x2_` and `y2_`: the end coordinates of the step.
# - `sl_`: the step length
# - `direction_p`: the direction of the step (relative to?)
# - `ta_`: the turn angle
# - `t1_` and `t2_`: the start and end time of a step.
# - `dt_`: the duration of a step.

s2 |> print(n = Inf)

# ... E: Exercise --- 
# Up to know we only considered one animal. Repeat the steps from above
# (resampling the track to a 4 hour sampling rate) and then create steps
# accounting for bursts for each animal. You may want to create a list column to 
# the object `trk1`.

# ... S: Solution ---

trk2 <- trk1 |> 
  # Specify the name of the new column (here data). All columns except id
  # will be put into this list. 
  nest(data = -id) |> 
  # We can use mutate to create new columns
  mutate(
    # First we create a new column with the resampled data. 
    # `map` iterates over a list (i.e., applies the function to each element in data). 
    # `~` can be thought of as: "do what ever comes afterwards to each element .x", 
    # where `.x` is the element currently under evaluation. 
    data.resampled = map(data, ~ track_resample(.x, rate = hours(4), tolerance = minutes(10))), 
    # We iterate again over a list (here `data.resampled`) and "just" apply the function 
    # `steps_by_burt()`
    # We could have also write this as: `map(data.resampled, ~ steps_by_burst(.x))`
    steps = map(data.resampled, steps_by_burst)) |> 
  # Finally, we just select the column with the steps and id.
  select(id, steps)



# ... E: Exercise --- 
# Use the `unnest()` function after and create a histogram of step-lengths (the column `sl_`)

# ... S: Solution ---
trk2 |> 
  # Make sure you specify the column that you want to unnest.
  unnest(cols = steps) |>  
  # `ggplot2` to create a histrogram. 
  ggplot(aes(sl_)) + geom_histogram() +
  theme_light()


# ... E: Exercise --- 
# Now create a histogram of step-lengths for each individual to check if 
# visually they move differently.


# ... S: Solution ---

trk2 |> 
  # Make sure you specify the column that you want to unnest.
  unnest(cols = steps) |>  
  # `ggplot2` to create a histrogram. 
  ggplot(aes(sl_)) + 
  geom_histogram() +
  theme_light() +
  facet_wrap(~id)
