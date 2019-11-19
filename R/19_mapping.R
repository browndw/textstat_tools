
# POP? SODA? or COKE?
#
# In 2013 a statistics (!) grad student at NC State named Joshua Katz
# put up a series of maps on an R Studio server that plotted the geographic distributions
# of lexical variants across the US. These turned out to be wildly popular
# and he published versions at Business Insider:
# https://www.businessinsider.com/american-english-dialects-maps-2018-1
# And at the New York Times, where he now works:
# https://www.nytimes.com/interactive/2014/upshot/dialect-quiz-map.html
#
# His maps are based on data collect by Bert Vaux and Scott Golder for the
# Harvard Dialect Survey in 2003:
# http://www4.uwm.edu/FLL/linguistics/dialect/index.html
#
# Katz's work is an interesting application of KNN to geospatial data.
# There is a nice introduction to the KNN algorithm here:
# https://www.analyticsvidhya.com/blog/2018/03/introduction-k-neighbours-algorithm-clustering/
#
# We're going to re-create Katz's pop vs. soda plot..

# This exercise requires a number of packages.
# A couple for data wrangling...
library(tidyverse)
library(data.table)

# Our plots will be based on the results of knn...
library(kknn) # for knn

# We have some realtively intensive processing tasks that can be sped up... 
library(foreach) # for iterating over elements in a collection
library(doParallel) # for parallel processing

# And we'll be working with spatial data, so we need some specialized packages...
library(sf) # for spatial data
library(rnaturalearth) # map data for plotting
library(housingData) # geolocation data

# Let's read in our data...
pvs_df <- read_csv("data/geo_data/pop_vs_soda.csv")

# Note how our data is structured...
pvs_df %>% head(10)

# We have counts by state and county. Importantly, those counts are identified by
# Federal Information Processing System (FIPS) codes.
# To convert that imformation to something mappable, we need GIS data for those
# FIPS codes. For that we can use data from the housingData package.
hd <- housingData::geoCounty
hd$fips <- as.character(hd$fips)

# Now let's select the columns we need from our data...
pvs_df <- pvs_df %>% select(fips = FIPS_combo, POP = SUMPOP, SODA = SUMSODA, 
                       COKE = SUMCOKE, OTHER = SUMOTHER)

# And put that data in a long format...
pvs_df <- pvs_df %>% gather(var, freq, -fips) %>% filter(freq != 0) 

pvs_df %>% arrange(fips) %>% head(10)

# For the task, we need to convert counts into instances.
# For example, if there are 3 "soda" preferers in a county, we want
# "soda", "soda", "soda" in the variable column. For that, we can
# use the data.table package...
setDT(pvs_df)
pvs_df <- pvs_df[rep(seq(.N), freq), !"freq"]

# Check the result...
pvs_df %>% arrange(fips) %>% head(10)

# Now let's join this with the GIS data to add longitudes and latitudes...
pvs_df <- left_join(pvs_df, select(hd, fips, lon, lat), by = "fips")

# Drop any empty rows...
pvs_df <- pvs_df[!is.na(pvs_df$lon), ]

# Check the result...
pvs_df %>% arrange(fips) %>% head(10)

# Now let's get the information we need for our map.
# We'll start with a map of the US states.
states_us <- rnaturalearthdata::states50

# There are several map options. We're going to select "iso_a2".
states_us <- states_us[states_us$iso_a2 == 'US',]

# And drop Alaska and Hawaii, so we'll only plot the Continental US.
states_us <- states_us[ !grepl( "Alaska|Hawaii" , states_us$name ) , ]

# Finally we want to convert this into an sf object for later plotting.
states_us <- st_as_sf(states_us)

# Now we'll similarly convert the coordinates in our pop vs. soda data.
# Note that we point the st_as_sf() function to our "lon" and "lat" columns.
# Additionally, we need to specify the coordinate reference system (crs).
# This can be tricky... See here for a discussion:
# https://ryanpeek.github.io/2017-08-03-converting-XY-data-with-sf-package/
# We're spcifying The World Geodetic System 1984 or WGS84.
point_data <- pvs_df %>% st_as_sf(coords = c("lon", "lat"),
                                 crs = "+proj=longlat +ellps=WGS84")


# Finally, we need to specify a projection. Remember we are "projecting" geographical
# space into 2 dimensions. So we have to decide what convetion to use.
# We'll use the North America Lambert Conformal Conic projection:
# https://en.wikipedia.org/wiki/Lambert_conformal_conic_projection
nalcc <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Now, we can transform our US data to fit this projection.
us <- states_us %>% st_transform(nalcc)

# And likewise our pop vs. soda data.
point_data <- point_data %>% st_transform(nalcc)

# Next, we'll make a data.frame with 3 columns...
dialects_train <- data.frame(dialect = point_data$var, 
                             lon = st_coordinates(point_data)[, 1], 
                             lat = st_coordinates(point_data)[, 2])

dialects_train %>% head(10)

# To get a sense of our data, we can take a random sample from our training data
# and plot it...
dialects_sample <- dialects_train %>% sample_n(1000)

ggplot(data = dialects_sample) +
  geom_sf(data = us) +
  geom_point(aes(x = lon, y = lat, color = dialect), alpha = 1) +
  scale_color_brewer(palette = "Dark2",  name = "Dialect")

# The implementation of the knn classifier is a little different from
# earlier classification problemns, so let's walk through the basic idea
# with a quick example. First, let's subset out the data for 2 counties:
# Allegheny and Philadelphia counties (identified by their FIPS codes).
dialects_penn <- point_data[grepl("42003|42101", point_data$fips), ]

# Then, we'll construct a simple data frame with 3 columns.
dialects_penn <- data.frame(dialect = dialects_penn$var, 
                            lon = st_coordinates(dialects_penn)[, 1], 
                            lat = st_coordinates(dialects_penn)[, 2])

# Finally, from that we can split the data into a training and test set.
set.seed(123)
valid_split <- rsample::initial_split(dialects_penn, .75)

penn_train <- rsample::analysis(valid_split)
penn_test <- rsample::assessment(valid_split)

# For more conventional classification tasks, we would want to determine
# the optimal k for our model using the train.kknn() function:
# https://rpubs.com/bonibruno/svm-knn
#
# For our purposes, we'll use a quick rule of thumb and use the 
# sqaure-root of our n observations.
# Now we can call a model in which we try to predict the dialect variant
# (soda, pop, coke, other) based on geolocation.
knn_penn <- kknn::kknn(dialect ~ ., 
                       train = penn_train, 
                       test = penn_test, 
                       kernel = "gaussian", 
                       k = 61)

# For convetional classification tasks, we would be interested in our 
# confusion matrix....
tab <- table(knn_penn$fitted.values, penn_test$dialect)

# And our model's accuracy...
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# For our purposes, we're interested in the probablities of each
# variant being preferred based on location.
# Allegheny county is at the top of our table, with a very strong preference
# for "pop"...
knn_penn$prob %>% head(1)

# And Philadelphia county is at the bottom of our table with a slightly
# less strong preference for "soda"...
knn_penn$prob %>% tail(1)

# Those probablilities can be used to select both color (for the most probable variant)
# and the alpha of the color (based on the value of the probablility).
#
# Sounds easy enough, but we don't have values for the entire map.
# We need to use our known values to estimate those at other unknown points.
# This is process called spatial interpolation:
# https://docs.qgis.org/2.18/en/docs/gentle_gis_introduction/spatial_analysis_interpolation.html
#
# What follows in wholely indebted to Timo Grossenbacher.
# https://timogrossenbacher.ch/2018/03/categorical-spatial-interpolation-with-r/
#
# Essentially, we need to configure an empty grid and interpolate that grid with
# our point data. Another complication is that this process is quite memory
# and computationally intensive. So we're ultimately going to split up that
# grid into batches.
#
# Let's start with a little clean up...
rm(dialects_penn, knn_penn, valid_split, penn_train, penn_test, accuracy)

# We'll begin setting up our grid by specifing its width in pixels.
width_in_pixels <- 300

# The width of a grid cell is calcuated from the dimensions
# of our US map and is defined as dx.
dx <- ceiling( (st_bbox(us)["xmax"] - 
                  st_bbox(us)["xmin"]) / width_in_pixels)

# The height of a grid cell is the same because we'll use quadratic grid cells, dx == dy
dy <- dx

# Calculate the height in pixels of the resulting grid.
height_in_pixels <- floor( (st_bbox(us)["ymax"] - 
                              st_bbox(us)["ymin"]) / dy)

# And finally construct the grid with the st_make_grid() function.
grid <- st_make_grid(us, cellsize = dx,
                     n = c(width_in_pixels, height_in_pixels),
                     what = "centers")

# And a little housekeeping...
rm(dx, dy, height_in_pixels, width_in_pixels)

# Next, we'll set our k at 1000. This will result is a smoother looking map.
k <- 1000

# Now, we can define a compute_grid() function that returns the probability of
# the most likely variant...
compute_grid <- function(grid, dialects_train, knn) {
  # create empty result data frame
  dialects_result <- data.frame(dialect = as.factor(NA), 
                                lon = st_coordinates(grid)[, 1], 
                                lat = st_coordinates(grid)[, 2])
  # run KKNN
  dialects_kknn <- kknn::kknn(dialect ~ ., 
                              train = dialects_train, 
                              test = dialects_result, 
                              kernel = "gaussian", 
                              k = knn)
  # bring back to result data frame
  # only retain the probability of the dominant dialect at that grid cell
  dialects_result <- dialects_result %>%
    # extract the interpolated dialect at each grid cell with the 
    # kknn::fitted() function
    mutate(dialect = fitted(dialects_kknn),
           # only retain the probability of the interpolated dialect,
           # discard the others
           prob = apply(dialects_kknn$prob, 1, function(x) max(x)))
  
  return(dialects_result)
}

registerDoParallel(cores = 4) # if you have more processing power you can up this number

# Specify number of batches and resulting size of each batch (in grid cells).
no_batches <- 60 # increase this number if you run into memory problems
batch_size <- ceiling(length(grid) / no_batches)

# Finally, we'll generate our result using parallel processing.
# This will take at least a couple of minutes...
dialects_result <- foreach(.packages = c("sf", "tidyverse"),
  batch_no = 1:no_batches, 
  # after each grid section is computed, rbind the resulting df into one big dialects_result df
  .combine = rbind, 
  # the order of grid computation doesn't matter: this speeds it up even more
  .inorder = FALSE) %dopar% {
    # compute indices for each grid section, depending on batch_size and current batch
    start_idx <- (batch_no - 1) * batch_size + 1
    end_idx <- batch_no * batch_size
    # specify which section of the grid to interpolate, using regular subsetting
    grid_batch <- grid[start_idx:ifelse(end_idx > length(grid), 
                                        length(grid),
                                        end_idx)]
    # apply the actual computation to each grid section
    df <- compute_grid(grid_batch, dialects_train, k)
  }


# We'll now convert the result into an sf object for mapping.
dialects_raster <- st_as_sf(dialects_result, 
                            coords = c("lon", "lat"),
                            crs = nalcc,
                            remove = F)

# Remeber that we've calculated a rectangular grid. As a last step, we can
# use the outline of the US to mask the borders of our map.
dialects_raster <- dialects_raster[us, ]

# And plot the result... (Note that we're dropping "other".)
ggplot(data = subset(dialects_raster, dialect != "OTHER")) +
  # add raster geom for the knn result
  geom_raster(aes(x = lon, y = lat, fill = dialect, alpha = prob)) +
  scale_fill_manual(values = c("forestgreen", "steelblue", "tomato")) +
  geom_sf(data = us, alpha = 0, size = 0.25) +
  theme_void() +
  theme(legend.position = "none")

# How does our result compare to Katz's?
# https://www.foodrepublic.com/2013/07/29/a-final-word-on-the-soda-vs-pop-vs-coke-nomenclature-debate/

