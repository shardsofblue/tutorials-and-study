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


#### 2.2.3 Basic map making ####

# Plot just these 4 attributes
plot(world[3:6])

# Plot just the population variable
plot(world["pop"])

# Plot Asia
world_asia <- world[world$continent == "Asia", ] # Just the Asian countries
asia <- st_union(world_asia) # Union the shapes into a single geometry
plot(world["pop"], # Can only have one facet variable
     reset = FALSE # Necessary because it has a key
     )
plot(asia, add = TRUE, col = "red")

# Plot population of countries
plot(world["continent"], # Plot the continent variabe, giving continents their own colors
     reset = FALSE) 
cex <- sqrt(world$pop) / 10000 # Calculate a diameter proportionate to the population
world_cents <- st_centroid(world, of_largest = TRUE) # Find the centers of each country
plot(st_geometry(world_cents), 
     add = TRUE, 
     cex = cex)

# Plot India in context with China
india <- world[world$name_long == "India", ]
plot(st_geometry(india), 
     expandBB = c(0, 0.2, 0.1, 1), # This expands the bounding box (bottom, left, top, right)
     col = "gray", 
     lwd = 3) # Line width
plot(world_asia[0], # Just geometry
     add = TRUE)

### Creating geometries from scratch ###
# POINT
plot(st_point(c(5, 2)), axes = T)                # XY point
#> POINT (5 2)
st_point(c(5, 2, 3))            # XYZ point
#> POINT Z (5 2 3)
st_point(c(5, 2, 1), dim = "XYM") # XYM point
#> POINT M (5 2 1)
st_point(c(5, 2, 3, 1))           # XYZM point
#> POINT ZM (5 2 3 1)


## MULTIPOINT
multipoint_matrix <- rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2)) # the rbind function simplifies the creation of matrices
plot(st_multipoint(multipoint_matrix))
#> MULTIPOINT (5 2, 1 3, 3 4, 3 2)
## LINESTRING
linestring_matrix <- rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
plot(st_linestring(linestring_matrix))
#> LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2)

## POLYGON
polygon_list <- list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
plot(st_polygon(polygon_list), axes=T)
#> POLYGON ((1 5, 2 2, 4 1, 4 4, 1 5))

## POLYGON with a hole
polygon_border <- rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole <- rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list <- list(polygon_border, polygon_hole)
plot(st_polygon(polygon_with_hole_list), axes=T)
#> POLYGON ((1 5, 2 2, 4 1, 4 4, 1 5), (2 4, 3 4, 3 3, 2 3, 2 4))

## MULTILINESTRING
multilinestring_list <- list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                            rbind(c(1, 2), c(2, 4)))
plot(st_multilinestring((multilinestring_list)), axes=T)
#> MULTILINESTRING ((1 5, 4 4, 4 1, 2 2, 3 2), (1 2, 2 4))

## MULTIPOLYGON
multipolygon_list <- list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
                         list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
plot(st_multipolygon(multipolygon_list), axes=T)
#> MULTIPOLYGON (((1 5, 2 2, 4 1, 4 4, 1 5)), ((0 2, 1 2, 1 3, 0 3, 0 2)))

## GEOMETRYCOLLECTION
gemetrycollection_list <- list(st_multipoint(multipoint_matrix),
                              st_linestring(linestring_matrix))
plot(st_geometrycollection(gemetrycollection_list), axes=T)
#> GEOMETRYCOLLECTION (MULTIPOINT (5 2, 1 3, 3 4, 3 2),
#>   LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2))


# sfc POLYGON
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2, crs = 4326)
st_geometry_type(polygon_sfc)
st_crs(polygon_sfc)


# Creating a geometry with attributes
lnd_point = st_point(c(0.1, 51.5))                 # sfg object, the geometry (a point)
lnd_geom = st_sfc(lnd_point, crs = 4326)           # sfc object (a column that can be appended to a data.frame)
lnd_attrib = data.frame(                           # data.frame object (attributes)
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object, the attributes and geometry combined

# Simple features are, in essence, data frames with a spatial extension
class(lnd_sf)

#### 2.3 Raster Data ####

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)

help("raster-package")

plot(new_raster)

# View which drivers are available
raster::writeFormats()
rgdal::gdalDrivers()

# Build a raster from scratch
new_raster2 = raster(nrows = 6, ncols = 6, 
                     res = 0.5, # Resolution
                     xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5, # centered on Prime Meridian & equator
                     vals = 1:36) # Fill the cells with values 1-36
plot(new_raster2)

### Multilayer rasters

# Raster brick
# Best for processing a single mulitilayer file or object
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
r_brick = brick(multi_raster_file)
nlayers(r_brick)

# Raster stack
# Allows calculations based on many files, many Raster* objects, or bot
raster_on_disk = raster(r_brick, layer = 1)
raster_in_memory = raster(xmn = 301905, xmx = 335745,
                          ymn = 4111245, ymx = 4154085, 
                          res = 30)
values(raster_in_memory) = sample(seq_len(ncell(raster_in_memory)))
crs(raster_in_memory) = crs(raster_on_disk)
r_stack = stack(raster_in_memory, raster_on_disk)
r_stack
nlayers(r_stack)

### 2.4.3 Coordinate Reference Systems (CRS) ###

# View available CRSs
crs_data = rgdal::make_EPSG()
View(crs_data)

# View and set vector CRSs
vector_filepath = system.file("vector/zion.gpkg", package = "spDataLarge")
new_vector = st_read(vector_filepath)
plot(new_vector["STATE"], axes=T)

st_crs(new_vector)
new_vector = st_set_crs(new_vector, 4326) # set CRS
plot(new_vector["STATE"], axes=T)

# View and set raster CRSs
projection(new_raster) # Get current CRS
projection(new_raster) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # set new CRS
plot(new_raster, axes=T)
projection(new_raster) = "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # set new CRS
plot(new_raster, axes=T)

### Units of measurement

luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg)
#> Note the inclusion of meters squared [m^2]
attributes(st_area(luxembourg))

# Convert to square kilometers
st_area(luxembourg) / 1000000 # Still says m^2
units::set_units(st_area(luxembourg), km^2)

# Raster projections do not store their units, so you must look them up



