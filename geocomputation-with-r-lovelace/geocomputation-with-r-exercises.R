### Excercises and notes from Geocomputation with R by Lovelace
###


library(sf)         # classes and functions for vector data
library(raster)     # classes and functions for raster data
library(spData)     # load geographic data (view loaded datasets: https://nowosad.github.io/spData/)
# devtools::install_github("Nowosad/spDataLarge")
library(spDataLarge)   # load larger geographic data
library(tidyverse)

###################
#### CHAPTER 2 ####
###################

#### 2.2 Vector Data

### CRS ###
# CRS coordinates consist of two numbers representing distance from an origin, usually in x then y dimensions
# Long/Lat values: c(-0.1, 51.5)
# Easting/Northing values of the British National Grid: c(530000, 180000)

### SF: Simple Features ###
# Supercedes sp, rgeos, and rgdal 
# points, lines, polygons and their respective ‘multi’ versions
# geometry collections

# Built-in tutorials for sf
vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package

# World is loaded with spData
head(world) # What's world look like
names(world) # spatial and attribute colunm names

# plot() is from sf
plot(world) # attribute facets of world, using the polys stored in world$geom
plot(world$geom) # just the geom polys

# The following two actions are equivalent ways of showing summary statistics of the lifeExp attribute.
# Note: Geometry cols are "sticky", kept unless the user deliberately removes them
summary(world["lifeExp"])
world %>% 
  select("lifeExp") %>%
  summary()

# View the first two rows and three columns of world
world[1:2, # rows
      1:3] #cols


### Backwards compatibility ###

# To convert an sf object to the Spatial class used in sp
library(sp)
world_sp <- as(world, Class = "Spatial")

# and back to sf
world_sf <- st_as_sf(world_sp)


### 2.2.3 Basic map making ###

# Plot just these 4 attributes
plot(world[3:6])

# Plot just the population variable
plot(world["pop"])



