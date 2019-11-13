### Introduction to visualising spatial data in R
### https://github.com/Robinlovelace/Creating-maps-in-R

# This file is based on the introductory PDF tutorial for the sp class system


# Load libraries
library(tidyverse) # fast and concise data manipulation packages
library(ggmap) # extends the plotting package ggplot2 for maps
#install.packages("sf", dependencies = T) # Make sure this is up to date
library(rgdal) # R’s interface to the popular C/C++ spatial data processing library gdal
library(rgeos) # R’s interface to the powerful vector processing library geos
library(maptools) # provides various mapping functions
library(tmap) # a package for rapidly creating beautiful maps
library(OpenStreetMap)
library(ggplot2)
library(leaflet)

###############
### Part II ###
###############

# Read in data
lnd <- readOGR(dsn = "data/london_sport.shp")
# equivalent to: readOGR(dsn = "data", layer = "london_sport")

# Look at the first 5 rows of the @data slot
head(lnd@data, n = 5)

# R remembers the previous slot
# short for mean(lnd@data$Partic_Per)
mean(lnd$Partic_Per) 

# Check classes of all slots in the dataset
sapply(lnd@data, class)
# Coerce Pop to a number
lnd$Pop_2001 <- as.numeric(as.character(lnd$Pop_2001)) 

# Count rows and cols in lnd@data
nrow(lnd)
ncol(lnd)

# Basic plot of lnd
plot(lnd)

# select rows of lnd@data where sports participation is less than 13
lnd@data[lnd$Partic_Per < 13, 1:3]

# Select zones where sports participation is between 20 and 25%
sel <- lnd$Partic_Per > 20 & lnd$Partic_Per < 25
plot(lnd[sel, ]) # output not shown here
head(sel) # test output of previous selection (not shown)

# Plot lnd and highlight areas that meet that criteria
plot(lnd, col = "lightgrey")
sel <- lnd$Partic_Per > 25
plot(lnd[ sel, ], col = "turquoise", add = TRUE) # add selected zones to map


# Find the centre of the london area
easting_lnd <- coordinates(gCentroid(lnd))[[1]]
northing_lnd <- coordinates(gCentroid(lnd))[[2]]

# arguments to test whether or not a coordinate is east or north of the centre
east <- sapply(coordinates(lnd)[,1], function(x) x > easting_lnd)
west <- sapply(coordinates(lnd)[,1], function(x) x < easting_lnd)
north <- sapply(coordinates(lnd)[,2], function(x) x > northing_lnd)
south <- sapply(coordinates(lnd)[,2], function(x) x < northing_lnd)

# test if the coordinate is east and north of the centre
lnd$quadrant <- "unknown" # prevent NAs in result
lnd$quadrant[east & north] <- "northeast"

plot(lnd, col = "lightgrey")
plot(lnd[east & north,], col = "turquoise", add = TRUE)
plot(lnd[east & south,], col = "red", add = TRUE)
plot(lnd[west & north,], col = "purple", add = TRUE)
plot(lnd[west & south,], col = "green", add = TRUE)





plot(lnd, col = "grey")
# find and store London's geographic centroid
cent_lnd <- gCentroid(lnd[lnd$name == "City of London",]) 
points(cent_lnd, 
       pch = 1, # shape of symbol (1=circle)
       cex = 5 # cex: symbol size based on fontsize of the device
       ) 

# set 10 km buffer
# Specifies the given geometry to include the area within the specified width
lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000) 
plot(lnd)
plot(lnd_buffer, add = T)

# method 1 of subsetting selects any intersecting zones
lnd_central <- lnd[lnd_buffer,] # the selection is large
plot(lnd_central)

plot(lnd)
# test the selection for the previous method - uncomment below
plot(lnd_central, col = "lightblue", add = T)
plot(lnd_buffer, add = T) # some areas just touch the buffer


# method2 of subsetting selects only points within the buffer
# Find centroids of all London areas
lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd)) # create spatialpoints
                           ) 
# Plot the centroids of all the boroughs
plot(lnd)
plot(lnd_cents, add=T)

# select only points inside buffer
sel <- lnd_cents[lnd_buffer,] 

# Plot the centroids of the boroughs inside the buffer
plot(lnd)
plot(sel, col = "blue", add=T) # add centroids to map

points( # Add points to map
  sel,
  pch = 19, # shape of symbol (1=circle, 19=filled circle)
  cex = 1, # cex: symbol size based on fontsize of the device) # show where the points are located
  col = "darkgreen"
  )

lnd_central <- lnd[sel,] # select zones intersecting w. sel

# Plot the boroughs just defined
plot(lnd_central, add = T, 
     col = "lightblue", 
     border = "grey")

# Show the buffer circle
plot(lnd_buffer, add = T, border = "red", lwd = 2)

# Add text to the plot
text(coordinates(cent_lnd), "Central\nLondon")

# Clear environment
rm(list=ls())

################
### Part III ###
################

vec <- vector(mode = "numeric", length = 3)
df <- data.frame(x = 1:3, 
                 y = c(1/2, 2/3, 3/4)
                 )
sp1 <- SpatialPoints(coords = df)
class(sp1)

spdf <- SpatialPointsDataFrame(sp1, data = df)
class(spdf)

# Read in data
lnd <- readOGR(dsn = "data/london_sport.shp")
plot(lnd)
proj4string(lnd) # check CRS information from lnd
proj4string(lnd) <- NA_character_ # remove CRS information from lnd
proj4string(lnd) <- CRS("+init=epsg:27700") # assign a new CRS
plot(lnd)

# View all possible CRS ESPG codes
EPSG <- make_EPSG() # create data frame of available EPSG codes
EPSG[grepl("WGS 84", EPSG$note), ] # search for WGS 84 code (for some reason note is empty?)

lnd84 <- spTransform(lnd, CRS("+init=epsg:4326")) # reproject
# Save lnd84 object (we will use it in Part IV)
saveRDS(object = lnd84, file = "data/lnd84.Rds")
rm(lnd84) # remove the lnd object
# we will load it back in later with readRDS(file = "data/lnd84.Rds")


# Create new object called "lnd" from "london_sport" shapefile
lnd <- readOGR("data/london_sport.shp")
plot(lnd) # plot the lnd object
nrow(lnd) # return the number of rows
names(lnd)

# Create and look at new crime_data object
crime_data <- read.csv("data/mps-recordedcrime-borough.csv",
                       stringsAsFactors = FALSE)
head(crime_data, 5)
head(crime_data$CrimeType, 5)

# Extract "Theft & Handling" crimes and save
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]
head(crime_theft, 2) # take a look at the result (replace 2 with 10 to see more rows)

# Calculate the sum of the crime count for each district, save result
crime_ag <- aggregate(CrimeCount ~ Borough, # aggregate col1 /by/ col2 (similar to group by)
                      FUN = sum, # Specify that col1 should be summed
                      data = crime_theft)
# Show the first two rows of the aggregated crime data
head(crime_ag, 5)

# Compare Name in lnd to Borough in crime_ag
lnd$name %in% crime_ag$Borough
# Return rows in lnd which do not match in crime_ag
lnd$name[!lnd$name %in% crime_ag$Borough] # Makes sense, since the City of London has it's own police force
plot(lnd)

# Return rows in crime_ag that don't match lnd
crime_ag$Borough[!crime_ag$Borough
                 %in% lnd$name]

crime_ag %>% filter(Borough == "NULL") # How many crimes took place in "NULL"?

head(lnd$name) # dataset to add to (results not shown)
head(crime_ag$Borough) # the variables to join


# Join crime data to the lnd data slot
lnd@data <- left_join(lnd@data, 
                      crime_ag, 
                      by = c('name' = 'Borough')
                      )
# Comfirm it worked
head(lnd@data)

# Plot the crime counts in the boroughs
qtm(lnd, "CrimeCount")

# Create new stations object using the "lnd-stns" shapefile.
stations <- readOGR(dsn = "data/lnd-stns.shp")

# Compare coordinate reference system (CRS)
proj4string(stations)
proj4string(lnd)

# The CRS do not match, so create reprojected stations object
stations <- spTransform(stations, CRSobj = CRS(proj4string(lnd)))

plot(lnd) # plot London 
points(stations) # overlay the station points

# Here we see the extent, 'bounding box', of stations goes well beyond that of lnd
bbox(stations)
bbox(lnd)

# Restrict the stations to those within lnd
stations <- stations[lnd, ] 

# Check it
plot(lnd)
plot(stations, add = T)

# Plot the stations in a viewable way
plot(lnd)
points(stations,
       pch = 19,
       cex = .25)


###############
### Part IV ###
###############

#### Using tmap ####

# Choropleth map
qtm(shp = lnd, 
    fill = "Partic_Per", # variable plotted
    fill.palette = "-Blues" # color palette
    )

# 2 choropleth maps at once
qtm(shp = lnd, 
    fill = c("Partic_Per", "Pop_2001"), 
    fill.palette = "Blues", 
    ncol = 2) 

# Plot each borough on its own facet
tm_shape(lnd) +
  tm_fill("Pop_2001", thres.poly = 0) +
  tm_facets("name", free.coords = TRUE, drop.units = TRUE)


# Transform the coordinate reference system
lnd_wgs = spTransform(lnd, CRS("+init=epsg:4326"))

#############################################################
# The following requires a Java environment =< Java6
# Run the following from the Terminal and restart RStudio
# 
# $ brew tap homebrew/cask-versions
# $ brew cask install java6
# $ sudo R CMD javareconf
# 
# Can install the most recent version of Java using:
#
# $ brew cask install java
#############################################################


# Plot on an OpenStreetMap basemap
if(curl::has_internet()) { # if you've got an internet connection...
  osm_tiles = tmaptools::read_osm(bbox(lnd_wgs)) # download images from OSM
  tm_shape(osm_tiles) + 
    tm_raster() +
    tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", 
            fill.title = "Population, 2001", 
            scale = 0.8,
            alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02)) 
} else { # otherwise...
  tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02))
}

#### Using ggmap ####

p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001))

p + 
  geom_point(aes(colour = Partic_Per, 
                 size = Pop_2001)) +
  geom_text(size = 2, aes(label = name))

## ggplot requires spatial objects be supplied as data.frames
lnd_f <- broom::tidy(lnd)

head(lnd_f, n = 2) # peak at the fortified data
lnd$id <- row.names(lnd) # allocate an id variable to the sp data
head(lnd@data, n = 2) # final check before join (requires shared variable name)
lnd_f <- left_join(lnd_f, lnd@data) # join the data
head(lnd_f)

map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) +
  geom_polygon() + 
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)",
       fill = "% Sports\nParticipation") +
  ggtitle("London Sports Participation") +
  scale_fill_gradient(low = "white", high = "black")
map

#### Using leaflet ####

# The leaflet package creates interactive web maps.
# Can be combined with Shiny

lnd84 <- readRDS('data/lnd84.Rds')

leaflet() %>%
  addTiles() %>%
  addPolygons(data = lnd84)


#### Faceting Maps #####

london_data <- read.csv("data/census-historic-population-borough.csv")
ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name)
head(ltidy, 2) # check the output
head(lnd_f, 2) 

# identify shared variables with ltidy 
ltidy <- rename(ltidy, ons_label = Area.Code) # rename Area.code variable
lnd_f <- left_join(lnd_f, ltidy)
head(lnd_f, 5)

# Rename the date variable
lnd_f$date <- gsub(pattern = "Pop_", replacement = "", lnd_f$date)

ggplot(data = lnd_f, # the input data
       aes(x = long, y = lat, fill = pop/1000, group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ date) + # one plot per time slice
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors
                       midpoint = 150, name = "Population\n(thousands)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks

# ggsave("figure/facet_london.png", width = 9, height = 9) # save figure




