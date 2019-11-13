# Aggregate file holding all
# Code lessons from: 
# R for Data Science
# By Hadley Wickham & Garrett Grolemund

##########################################################################

# Chapter 1: Data Visualization with ggplot2

# Started Dec. 14, 2018
# By Roxanne Ready

# Load packages
library(tidyverse)

##########################################
## Inspect the pre-built mpg data frame ##
##########################################

mpg
View(mpg)
# 11 variables
# 234 rows

?mpg
# city/hwy = mpg on city/hwy roads
# cyl = num cylinders
# displ = engine size in liters
# fl = fuel type
# drv= front/rear/4 wheel drive

t <- summary(mpg)
View(t)
# 1999-2008
# avg mpg city: 16.86
# avg mpg hwy: 23.44

####################
## Begin plotting ##
####################

# Create first plot
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy))

###############
## TEMPLATES ##
###############

# Template for basic plot creation
# ggplot(data = <DATA>) +
#   <GEOM_FUNCTION>(mapping = aes(<XY_MAPPINGS>, <OTHER_MAPPINGS>), <MANUAL_AESTHETICS>)

# Updated template for plot creation (<<>> indicates required parameter)
# ggplot(data = <<DATA>>) +
#   <<GEOM_FUNCTION>>(
#     mapping = aes(<<XY_MAPPINGS>>, <OTHER_MAPPINGS>),
#     stat = <STAT>,
#     position = <POSITION>
#     ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>

######################
## Aesthetic levels ##
######################

# scaling by color for class
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color=class))

# Other scaling options:
#size
#shape (maximum of 6)
#alpha (transparency)

# scaling by two levels
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color=class, alpha=displ))

# setting the color manually
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy), color="blue")

# Aesthetic manual settings:
#color as a string or #RRGGBB
#size in mm
#shape as a number

# Investigating geom_point aesthetics
?geom_point
vignette("ggplot2-specs")

# Facets break the plot into multiple plots by chosen variables
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_wrap(~ class, nrow=2)

ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_wrap(~ drv, nrow=2)

ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(drv ~ .)

?facet_wrap

# Multiple geoms in a single plot
# with duplication
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  geom_smooth(mapping = aes(x=displ, y=hwy))

# redundancy removed
ggplot(data=mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point() + 
  geom_smooth()

# overwrite added to one geom
ggplot(data=mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(color=class)) + 
  geom_smooth()

# filter added to one geom
ggplot(data=mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(color=class)) + 
  geom_smooth(
    data=filter(mpg,class=="subcompact"),
    se=FALSE
  )

###############################################
## Inspect the pre-built diamonds data frame ##
###############################################

diamonds
View(diamonds)
# 10 variables
# 53,930 rows

?diamonds
# price - price in US dollars (\$326–\$18,823)
# carat - weight of the diamond (0.2–5.01)
# cut - quality of the cut (Fair, Good, Very Good, Premium, Ideal)
# color - diamond colour, from J (worst) to D (best)
# clarity - a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
# x - length in mm (0–10.74)
# y - width in mm (0–58.9)
# z - depth in mm (0–31.8)
# depth - total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)
# table - width of top of diamond relative to widest point (43–95)

t <- summary(diamonds)
View(t)
# price: $326 - $18,823, avg $3,933
# carat: .2 - 5.01, avg .8

#################################
## Statistical Transformations ##
#################################

# Bar plot
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut))

# Under the hood, geom_bar transforms the data using stat_count() and outputs those values as the y axis
# The below returns the same as the above
ggplot(data=diamonds) +
  stat_count(mapping=aes(x=cut))

# Every geom has a default stat, 
# and every stat has a default geom.

# Override default stat for geom_bar with a different y value (functional code, but meaningless output)
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, y=carat), stat="identity")

# Override deafult geom_bar stat with proportion instead of count
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, y=..prop.., group=1))

# Compute a summary plot showing max, min and average values within each category
ggplot(data=diamonds) +
  stat_summary(
    mapping = aes(x=cut, y=depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

########################
## Color and Position ##
########################

# Color the bars by type
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, fill=cut))

# Stack the bars by a third variable
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, fill=clarity))

# POSITION: IDENTITY
# View the bars as transparent overlaps (working code but more useful for points than bar geoms)
ggplot(data=diamonds,
       mapping=aes(x=cut, fill=clarity)) +
  geom_bar(alpha=1/5, position = "identity")

# View the bars as overlapping outlines (even less useful than the above for bar geoms)
ggplot(data=diamonds,
       mapping=aes(x=cut, color=clarity)) +
  geom_bar(fill=NA, position = "identity")

# POSITION: FILL
# Equalize the bar heights to compare proportion
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, fill=clarity),
           position="fill")

# POSITION: DODGE
# Place the bars for each type beside each other
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, fill=clarity),
           position="dodge")

# POSITION: JITTER
# Spread the dots on a scatterplot apart slightly to see when there are a number of them in one place
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy),
             position="jitter")

# geom_jitter accomplishes the same thing:
ggplot(data=mpg) +
  geom_jitter(mapping=aes(x=displ, y=hwy))

########################
## Coordinate Systems ##
########################

# Flip the x and y axes (especially useful for long labels)
ggplot(data=mpg, mapping=aes(x=class, y=hwy)) +
  geom_boxplot() +
  coord_flip()

# Coxcomb (proportional pizza) chart
ggplot(data=diamonds) + 
  geom_bar(
    mapping=aes(x=cut, fill=cut),
    show.legend = FALSE,
    width=1
  ) +
  theme(aspect.ratio=1) +
  labs(x=NULL, y=NULL) +
  coord_flip() +
  coord_polar()

##########################################################################

# Chapter 3: Data Transformation with dplyr

# Started 
# By Roxanne Ready

# Load packages
#install.packages("nycflights13")
library(tidyverse)
library(nycflights13)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtn:        run code line/block
# cmd-opt-e:      run code until EoF

# Inspect the dataframe
??nycflights13
?flights

t <- flights
View(t)
# 336,776 records
# 19 variables

##########
# FILTER #
##########

(dec25 <- filter(flights, month==12, day==25))

# Use == to TEST FOR EQUALITY, use = to MAKE ASSIGNMENTS
# Use near() to test mathematical equality
sqrt(2)^2 == 2 #FALSE
near(sqrt(2)^2, 2) #TRUE

# &, |, ! and IN
(filter(flights, month == 11 | month ==12)) # YES; Flights in Nov. and Dec.
(filter(flights, month == 11 | 12)) # NO; Flights in (TRUE which == 1 which == Jan.)
(filter(flights, month %in% c(11,12))) # YES; Flights in Nov. and Dec.

# Notes on NA values
x <- NA
is.na(x)

# Filter excludes FALSE and NA values

###########################
# Arrange, Select, Mutate #
###########################

# Arrange
arrange(flights, desc(arr_delay)) # Reorders the dataframe by arrival delay, descending

# Select
?select
select(flights, year, month, day) # Select year, month, and day
select(flights, year:day) # Select year through day
select(flights, -(year:day)) # Select everything except year through day

# helper functions for use in selecting variables (not filtering variables)
starts_with("abc")
ends_with("abc")
contains("abc")
matches("(.)\\1") # matches against regular expressions
num_range("x", 1:3)

(select(flights, starts_with("sched")))

(rename(flights, tail_num = tailnum)) # rename a variable
(select(flights, tailnum))

(select(flights, time_hour, air_time, everything())) # move variables to the beginning of the frame

# Mutate
# create a subset dataframe
flights_sm <- select(flights,
                     year:day,
                     ends_with("delay"),
                     distance,
                     air_time)

# add two variables
mutate(flights_sm,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

View(flights_sm) # the original is not touched; data is not stored unless it is assigned

# store the data in a new frame
flights_sm2 <- mutate(flights_sm,
                      gain = arr_delay - dep_delay,
                      speed = distance / air_time * 60)

View(flights_sm2) # here is the new data

# keep only the gain information, store in a new field
flights_gains_info <- transmute(flights,
                                gain = arr_delay - dep_delay,
                                hours = air_time / 60,
                                gains_per_hour = gain / hour
)

View(flights_gains_info)

# Some useful functions
x / sum(x) # compute proportion of a total
y - mean(y) # compute the difference from the average

# Splitting out integers by decimal places
transmute(flights,
          dep_time,
          hour = dep_time %/% 100, # integer division "shifts the decimal point" to pull out the hour
          minute = dep_time %% 100 # remainder does the same in the other direction to pull out the minutes
)

log2(x) # computes a log 2; a difference of 1 corresponds to doubling, -1 to halving

# Leads and lags
lead(x)
(x<-1:10)
lead(x)
lag(x)
x-lead(x)
(x != lag(x))

# Cummulative aggregates
cumsum(x)
cumprod(x)
cummin(x)
cummax(x)
cummean(x)

# Ranking
(x<-c(1, 1, 4, 10, 3, 1, 8, 12, 1))
min_rank(x)
min_rank(desc(x))

#############
# Summarise #
#############
#summarize() or summarise()

# This tells what the mean delay time is for every dep_delay in the dataframe, collapsed into a single result
summarise(flights, delay = mean(dep_delay, na.rm=TRUE))

# This tells what the mean delay time is for each unique date in the dataframe, resulting in 365 results
(by_day <- group_by(flights, year, month, day))    
summarise(by_day, delay = mean(dep_delay, na.rm=TRUE)) 

################
# The Pipe %>% #
################
# The pipe cascades operations to remove the need for storing multiple variables for each step in a process
# na.rm removes NA values so the summary statistics don't return NA

# Without the pipe:
by_dest <- group_by(flights, dest) # group flights dataframe by destination
delay <- summarise(by_dest, # summary statistics:
                   count = n(), # number
                   dist = mean(distance, na.rm=TRUE), # average distance
                   delay = mean(arr_delay, na.rm=TRUE) # average delay
)
delay <- filter(delay, count > 20, dest != "HNL") # remove low-travel areas and outlier HNL
View(delay)

# With the pipe:
delays <- flights %>% # choose the flights dataframe
  group_by(dest) %>% # group flights by destination
  summarise( # summary statistics for what was generated in the previous step
    count = n(), 
    dist = mean(distance, n.rm=TRUE),
    delay = mean(arr_delay, na.rm=TRUE)
  ) %>%
  filter(count > 20, dest != "HNL") # filter what was generated in the previous step
View(delays)

# A ggplot of the information
(ggplot(data = delays, mapping = aes(x=dist, y=delay)) +
    geom_point(
      aes(size = count), 
      alpha = 1/3) +
    geom_smooth(se = FALSE))

# Instead of using na.rm over and over, we can store a frame without the NA values, in this case representing a data frame with no cancelled flights.
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

# Plotting with counts
# create a data frame showing delays of individual planes (tailnums) by number of flights (n)
delays <- not_cancelled %>%
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm=TRUE),
    n = n()
  )

# plot the frame
ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

# We can see that the more flights a plane has, the lower the variance in delay times, a common shape

# filter out planes with fewer than 25 flights, to get a better picture of the data
delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

##########################################################################

# Chapter 5: Exploratory Data Analysis

# Started Dec. 19, 2018
# By Roxanne Ready

# Load packages
#install.packages("nycflights13")
#install.packages("hexbin")
library(tidyverse)
library(hexbin)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtn:        run code line/block
# cmd-opt-e:      run code until EoF

################################
# Notes on working directories #
################################

# HOW TO SET WORKING DIR

# Set working directory
# setwd("/Volumes/Passport/Programming-Lessons/r-for-data-science-lessons")

# BUT instead of using setwd(), use RStudio's built-in Projects support. This sets the working directory without explicitly stating as much in the code.

# NOTES ON SYNTAX

# RStudio understands forward or backward slashes for directory pathing, regardless of the native environment in which it's running (Win uses backslashes). But, R needs backslashes typed twice to understand them, so use forward slashes (/) at all times to avoid this.

# Absolute paths: Win uses two backslashes, Mac uses one forward slash, but using either in scripts should be avoided because everyone's working directory will be different.

# ~ points to home directory on Mac, and My Documents on Win.

#############################
# Visualizing Distributions #
#############################

# Categorical variables can take only a set of values; factors or characters

# count of diamonds by cut
diamonds %>%
  count(cut)

# visualization of count of diamonds by cut
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut))

# Continuous variables can take an infinite set of ordered values; e.g. numbers, date-times

# count of distribution of carat value grouped in sections .5 wide
diamonds %>%
  count(cut_width(carat, 0.5))

# visualization of distribution of carat value
ggplot(data=diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# zoomed in visualization shows only diamonds of < 3 carats (zoom in on the graph, since very few diamonds are > 3 carats), grouped in sections .1 wide
smaller <- diamonds %>%
  filter(carat < 3)

ggplot(data=smaller, mapping=aes(x=carat)) +
  geom_histogram(binwidth=0.1)

# Use geom_freqpoly() split by color to overlay histogram data (same data, plotted as lines instead of bars)
ggplot(data = smaller, mapping = aes(x=carat, color=cut)) +
  geom_freqpoly(binwidth = 0.1)

######################
# Exploring the data #
######################

ggplot(data=smaller, mapping=aes(x=carat)) +
  geom_histogram(binwidth=0.01)

?diamonds
# y is the width in mm

ggplot(diamonds) +
  geom_histogram(mapping=aes(x=y), binwidth=0.5)
# Notice the tiny blip at 0 and the wide range of the x-axis, even though the bars are too short to actually make sense of the outliers.

#zoom in on small y-axis values by limiting the height of the y-axis
ggplot(diamonds) +
  geom_histogram(mapping=aes(x=y), binwidth=0.5) +
  coord_cartesian(ylim=c(0,20))
# Now I can see the approximate values of y (the x-axis) that are outliers, at 0, around 30 and around 60.

#pull out the outliers into their own data frame
unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>% 
  arrange(y)
unusual
# This shows 7 diamonds that have a width of 0, probably an error.
# Also shows 2 diamonds more than an inch long at unreasonbly low prices, probably also an error.

#####################
# Handling outliers #
#####################

# Option 1: drop the row
diamonds_dropped <- diamonds %>%
  filter(between(y, 3, 20))
diamonds_dropped

#verify the outliers are gone by repeating line 69 with the new tibble
ggplot(diamonds_dropped) +
  geom_histogram(mapping=aes(x=y), binwidth=0.5) +
  coord_cartesian(ylim=c(0,20))

# Option 2: replace the outlier values with NA, preserving the rest of the row
diamonds_na <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
diamonds_na

#verify the outliers are gone by repeating line 69 with the new tibble
ggplot(diamonds_na) +
  geom_histogram(mapping=aes(x=y), binwidth=0.5) +
  coord_cartesian(ylim=c(0,20))

# Comparing NA against other variables
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time), # Make a new variable to define NA values
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  # plotted by a freqpoly
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(
    mapping = aes(color = cancelled),
    binwidth = 1/4
  ) # difficult to make use of because there are so few cancelled flights

# change out the y-axis on a freqpoly from count to density, to make results more meaningful
ggplot(
  data = diamonds,
  mapping = aes(x = price, y = ..density..) # compare prices as density (not count)
) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500) # split out by cut to compare cuts against each other

# look diamonds price distribution by boxplot
ggplot(
  data = diamonds,
  mapping = aes(x = cut, y = price)
) +
  geom_boxplot()

# viewing variation across unordered variables
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

# make the trend easier to see by reordering the unordered variable against something else
ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median), # reorder class based on median value of hwy
      y = hwy
    )
  )

# flip the boxplot so the variable names don't overlap each other
ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median), # reorder class based on median value of hwy
      y = hwy
    )
  ) +
  coord_flip()

# EXERCISES
#1. Remake the cancelled flight plot with a boxplot
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time), # Make a new variable to define NA values
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  # plotted by a boxplot
  ggplot() +
  geom_boxplot(
    mapping = aes(
      x = cancelled,
      y = sched_dep_time
    )
  )

#2. 
# I want to compare size to price, but both are continuous variables.
# Previously, I had compared cut (categorical) to price (continuous)
diamonds

ggplot(
  data = diamonds,
  mapping = aes(x = cut, y = y)
) +
  geom_boxplot() +
  coord_cartesian(ylim=c(2.5,10))

ggplot(
  data = diamonds,
  mapping = aes(x = y, y = ..density..) # compare prices as density (not count)
) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = .5) +
  coord_cartesian(xlim=c(4,9), ylim=c(0,.5))

View(summary(diamonds))

ggplot(
  data = diamonds,
  mapping = aes(x = y, y = ..density..) # compare prices as density (not count)
) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = .75) +
  coord_cartesian(xlim=c(4,9), ylim=c(0,.5))

?geom_count

d <- ggplot(diamonds, aes(y, price)) + xlim(4, 10)
d + geom_bin2d()

ggplot(
  data=diamonds,
  mapping = aes(x=y, y=price, color=cut), position="jitter"
) +
  geom_point() +
  xlim(5,10)

ggplot(
  data=diamonds,
  mapping = aes(x=y, y=price, color=cut), position="jitter"
) +
  geom_point() +
  xlim(5,10)

########################
# Visualize 2 cat vars #
########################

# geom_count shows count of each variable intersection
ggplot(data=diamonds) +
  geom_count(mapping = aes(x=cut, y=color))

# dplyer can show the count numerically
diamonds %>%
  count(color, cut) %>%
  arrange(n)

# geom_tile can visualize the dplyer count
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping=aes(x=color, y=cut)) +
  geom_tile(mapping=aes(fill=n))

#########################
# Visualize 2 cont vars #
#########################

# scatterplot
ggplot(data = diamonds) +
  geom_point(
    mapping = aes(
      x = carat, 
      y = price)
  )

# add transparency for easier reading
ggplot(data = diamonds) +
  geom_point(
    mapping = aes(
      x = carat, 
      y = price),
    alpha = 1/100
  )

# Bin in 2 dimensions using geom_bin2d() or geom_hex()
ggplot(data = smaller) +
  geom_bin2d( #bin2d
    mapping = aes(
      x = carat,
      y = price
    )
  )

ratio_display <- 1/1
ratio_values <- (max(smaller$x)-min(smaller$y))/(max(smaller$y)-min(smaller$y))
ggplot(data = smaller) +
  geom_hex( #hex
    mapping = aes(
      x = carat,
      y = price
    )
  ) 

# Bin one continuous variable so it can be plotted as if it were categorical
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + # define the continuous variables here
  geom_boxplot(mapping = aes(
    group = cut_width(carat, 0.1) # bin one of the continuous variables using width of .1
  ))

# Do the same, but cutting each bin by the same number of points, isntead of a width
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + # define the continuous variables here
  geom_boxplot(mapping = aes(
    group = cut_number(carat, 20) # bin one of the continuous variables, filling each with 20
  ))

##########################################################################

# Chapter 7: Tibbles with tibble

# Started Dec. 19, 2018
# By Roxanne Ready

# Load packages
#install.packages("nycflights13")
#install.packages("hexbin")
library(tidyverse)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtn:        run code line/block
# cmd-opt-e:      run code until EoF

##########################################

#########################
# Understanding tibbles #
#########################

# Convert from data.frame to tibble
iris # data.frame
(as_tibble(iris)) # convert to and view tibble

# Create a tibble from scratch
(tibble(
  x = 1:5, # fill variable x with 1 to 5
  y = 1, # fill variable y with 1
  z = x^2 + y # fill variable z with x squared plus 1
))

# Unlike data.frame(), tibble() does not convert inputs (such as strings to factors), change the name of a variable, or create a row name.

# Tibbles show the data type at the top of each column.

# If a variable name is nonsyntactic — does not start with letter or contain unusual characters — surround it with backticks ` to refer to it.
(tibble(
  `:)` = 1:5,
  `Don't use spaces` = 1,
  `1 bad n@me` = `:)`^2 + `Don't use spaces`
))

# tribble() also creates a tibble, but puts it in a format that's easy to read in code. (It stands for transposed tibble, not a grain-eating alien furball.)
(tribble(
  ~x, ~y, ~z,
  #--/--/--- This line isn't necessary but makes a nice visual distinction between the header and other rows
  "a", 2, 3.6,
  "b", 1, 8.5
))

# By default tibbles only show the first 10 rows and the columns that fit in the console window, whereas basic data.frames can overwhelm the console by displaying entire datasets.
nycflights13::flights

# To show more columns or rows, use print() explicitly...
nycflights13::flights %>%
  print(n=11, width=Inf)

# ...or use RStudio's View() command.
View(nycflights13::flights)

##############
# Subsetting #
##############

# Subset data using $ (to deliniate by name) or [[]] (to deliniate by name or position)

# Create a demo tibble using RNG
(df <- tibble(
  x = runif(5),
  y = rnorm(5)
))

# Extract by name
df$x
df[["x"]]

# Extract by position
df[[1]]

# In a pipe %>% you need to use .
# Extract in a pipe by name
df %>%
  .$x

# Extract in a pipe by position
df %>%
  .[[1]]

########################################
# Converting from tibble to data.frame #
########################################

df # a demo data frame tibble created earlier
class(as.data.frame(df)) # check the class when using as.data.frame
(as.data.frame(df)) # see what it looks like in the console using as.data.frame
df # note it hasn't actually stored the change
df_untibble <- as.data.frame(df) # store the tibble as a basic data.frame
df_untibble # view the stored data.frame

##########################################################################

# Chapter 8: Data Import with readr

# Started Dec. 19, 2018
# By Roxanne Ready

# Load packages
#install.packages(c("nycflights13", "gapminder", "Lahman"))
#install.packages("hexbin")
#install.packages("feather")
library(tidyverse)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtn:        run code line/block
# cmd-opt-e:      run code until EoF

##########################################

# readr is part of the tidyverse
#   read_csv() for ,
#   read_csv2() for ;
#   read_tsv() for tab
#   read_delim() for any delim
#   read_fwf() for fixed-width
#   read_table() for fixed-width separated by white space
#   read_log() with webreadr for Apache-style log files

# Import a csv file
# The first row will be the col names
heights <- read_csv("data/heights.csv")
heights

# Read in an inline csv
# The first row will be the col names; note the lack of comma at end of each row
test <- read_csv("a, b, c
                 1, 2, 3
                 4, 5, 6")
test

# Skip the first n rows of a csv (good in cases of metadata clutter)
(read_csv("Some metadata on line 1
          Another line of metadata
          x, y, z
          a, b, c",
          skip = 2))

# If the data has no col names, col_names = FALSE tells readr to assign sequential labels X1:Xn
(read_csv("x, y, z
          a, b, c",
          col_names = FALSE))

# Can assign colnames manually. Can also use \n instead of actual new line
(read_csv("1,2,3\n4,5,6", 
          col_names=c("a","b","c")))

# Some datasets use something other than "NA" to deliniate missing values. Tell readr that with na = ""
(read_csv("a,b,c\n4,.,6", 
          na = "."))

# Base R uses read.csv
#   read_csv is ~10x faster & includes a progress bar
#   read_csv produces tibbles (so it also doesn't convert chars to factors or use row names)
#   read.csv (and all Base R functions) inherit some behavior from the system environment they are run in; read_csv does not

##################
# Vector Parsing #
##################

# Numbers with varying decimal markers
parse_double("1.23") # US-style decimal deliniator is default
parse_double("1,23") # This won't work...
parse_double("1,23", locale = locale(decimal_mark=",")) # ...unless you tell it "," is the decimal marker

# Numbers with contextual non-numeric characters attached
parse_number("$100")
parse_number("20%")
parse_number("Price: ¥50")

# Numbers with varying grouping markers
parse_number("$1,253,098") # US = default
parse_number("1.253.098", locale = locale(grouping_mark=".")) # EU, must be specified
parse_number("1'253'098", locale = locale(grouping_mark="'")) # Switzerland, must be specified

# Understanding characters
charToRaw("Hadley") # Will output ASCII code

# R assumes UTF-8 by default, which is usually sufficient but will fail if the data was produced before UTF-8 became the standard. To specify, use locale().
parse_character("El Ni\xf1o", locale = locale(encoding = "Latin1")) # aka ISO-8859-1
parse_character("\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd", locale=locale(encoding = "Shift-JIS"))

# guess_encoding(charToRaw(x)) can attempt to guess the encoding of x, either a path or a direct input. Works better with long strings.
guess_encoding(charToRaw("El Ni\xf1o"))

# Factors represent categorical values with known levels
fruit <- c("apple", "banana")
parse_factor(c("apple", "orange", "apple", "banana"), levels = fruit)
parse_factor(c("apple", "apple", "apple", "banana"), levels = fruit)

# Date, date-time, time
parse_datetime("2010-10-01T2010") # Expects ISO-8601, ordered year, month, day, hour, minute, second. Can parse without time; sets to midnight.
parse_date("2010-10-01") # Expects xxxx-xx-xx or xxxx/xx/xx
library(hms) # supplements Base R's time support
parse_time("01:10 am") # Expects hour:min[:seconds][am/pm]

# can build own date-time format (p. 135)

################
# File Parsing #
################

# When reading a file, readr guesses at the column types
guess_parser("1,234,567") # Shows what readr decides
parse_guess("1,234,567") # Outputs the input parsed based on its guess

# Challenge file
challenge <- read_csv(readr_example("challenge.csv")) # This is the default. It will guess at col types.
challenge
tail(challenge)

# Explicitly set col types
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(), # explicitly set the col named x to type double
    y = col_date() # every parse_ function has an equivalent col_ function
  )
)

# Always explicitly set col types for maximum reproducability and to reduce failure possibilities if the underlying data changes.
# use stop_for_problems() to halt the script for each error.

# Increase the rows readr uses to guess
challenge <- read_csv(
  readr_example("challenge.csv"),
  guess_max = 1001
)

# Read in all columns as char...
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(.default = col_character())
)
challenge

# ...then convert them after import
type_convert(challenge) # as defaults or
type_convert(challenge, # by explicit assignment
             col_types = cols(
               x = col_double(),
               y = col_character())
)

########################
# Outputting to a file #
########################

# write_csv()
# write_tsv()
# both encode using UTF-8 and save date-times in ISO-8601

# write_excel_csv()
# adds a mark to the file header to tell Excel it uses UTF-8

# Save the df to a csv
write_csv(challenge, "data/challenge.csv")

# Note when reloading...
read_csv("data/challenge.csv") 
# ...that the col type information is lost and must be reset as above.

# Saving the df to R's proprietary format preserves col types
write_rds(challenge, "data/challenge.rds")
read_rds("data/challenge.rds") # Check that it preserves col types

# Saving the df to feather, a binary format, preserves col types and allows sharability across programming languages. It is also faster.
library(feather)
write_feather(challenge, "data/challenge.feather")
read_feather("data/challenge.feather")

##############################
# Importing other data types #
##############################

# Other tidyverse packages:

# haven reads SPSS, Stata and SAS
# readxl reads .xls and .xlsx
# DBI allow running SQL queries against a database to return a data frame when used with
#   RMySQL
#   RSQLite
#   RPostgreSQL
#   etc.
# jsonlite reads JSON
# xml2 reads XML
# rio plus R data import/export manual for others

##########################################################################

# Chapter 9: Tidy Data with tidyr

# Started Dec. 20, 2018
# By Roxanne Ready

# Load packages
#install.packages(c("nycflights13", "gapminder", "Lahman"))
#install.packages("hexbin")
#install.packages("feather")
library(tidyverse)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtn:        run code line/block
# cmd-opt-e:      run code until EoF

##########################################

#####################
# Spread and Gather #
#####################

# Create an un-tidy data frame
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)
preg

# I want it to look like this when I'm done:
# gender  count_preg count_not_preg   total
# male    NA         20               20
# female  10         12               32

# gather so it is one-dimensional instead of two
tidy_preg1 <- preg %>%
  gather(male, female, key = "gender", value = "count")
tidy_preg1

# spread and finanlize
tidy_preg2 <- tidy_preg1 %>%
  spread(pregnant, value = count) %>% # spread to make yes/no a variable
  rowwise() %>% # tell R to handle following arguments by row
  mutate(total = sum(no, yes, na.rm = TRUE)) # add a total summary row, ignore NA
tidy_preg2

######################
# Separate and Unite #
######################

# Create a table to play with
sepuni <- tribble(
  ~col1, ~col2,
  4, "10/33",
  20, "12/91"
)
sepuni

sep <- sepuni %>%
  separate(
    col2, # Which column to separate
    into = c("A", "B"), # Name the columns
    sep = "/", # Specify the separator; by default it picks the first non-alphanumeric character
    convert = TRUE # Use its best guess to convert the new columns
  )
sep

uni <- sep %>%
  unite(
    AB, # Name the new column
    A, B, # Which columns to combine
    sep = "!" # What to place between unified values. By default, a "_". Can use "" for no separator.
  )
uni

##################
# Missing Values #
##################

# Create a table with implicit and explicit missing values (2015 qtr 4 and 2016 qtr 1)
(stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(    1,    2,    3,   4,    2,    3,    4),
  return = c( 1.88, 0.59, 0.35,  NA, 0.92, 0.17, 2.66)
))

# Spreading the dataset shows that there are two missing values, even though only one is explicitly stated in the original dataset.
(stocks %>%
    spread(year, return))

# To eliminate explicit missing values, spread and re-gather the data with NAs removed
(stocks %>%
    spread(year, return) %>%
    gather(year, return, `2015`:`2016`, na.rm = TRUE))

# To make the implicit values explicit, let complete() analyze the dataset and determine them by pattern
(stocks %>%
    complete(year, qtr))

# Create a table with explicit missing values meant to be carried forward, as from data entry.
(stocks2 <- tibble(
  year   = c( 2015,   NA,   NA,  NA, 2016,   NA,   NA),
  qtr    = c(    1,    2,    3,   4,    2,    3,    4),
  return = c( 1.88, 0.59, 0.35, 1.2, 0.92, 0.17, 2.66)
))

# Carry the last observation forward using fill()
(stocks2 %>%
    fill(year))

######################
# Case Study: WHO db #
######################

who

who1 <- who %>%
  gather(
    new_sp_m014:newrel_f65, key = "key",
    value = "cases",
    na.rm = TRUE
  )
who1

(who1 %>%
    count(key))

?who
# rel = relapse, sn = negative pulmonary smear, sp = positive pulmonary smear, ep = extrapulmonary) to a code for gender (f = female, m = male) to a code for age group (014 = 0-14 yrs of age, 1524 = 15-24 years of age, 2534 = 25 to 34 years of age, 3544 = 35 to 44 years of age, 4554 = 45 to 54 years of age, 5564 = 55 to 64 years of age, 65 = 65 years of age or older

# In the data, newrel does not follow the convention of including _ between new and the code, so make that consistent.
who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

# Pass #1 of separate breaks the data at the _
who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

(count(who3, new)) # Confirms that all values of new are "new"

who4 <- who3 %>%
  select(-new, -iso2, -iso3) # Drop redundant columns

who5 <- who4 %>%
  separate(sexage, c("sex","age"), sep = 1) # Split the sex and age (split after the first char)
who5

# Put all together as a long pipe:
who_tidy <- who %>%
  gather(new_sp_m014:newrel_f65, 
         key = "code",
         value = "cases",
         na.rm = TRUE) %>%
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "type", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)
who_tidy

##########################################################################

# Chapter 10: Relational Data with dplyr

# Started Dec. 22, 2018
# By Roxanne Ready

# Load packages
#install.packages(c("nycflights13", "gapminder", "Lahman"))
library(tidyverse)
library(nycflights13)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtn:        run code line/block
# cmd-opt-e:      run code until EoF

##########################################

airlines
airports
planes
weather

############################
# Identifying primary keys #
############################

# PLANES: Check for tailnums listed more than once in the planes table
planes %>%
  count(tailnum) %>%
  filter(n>1)

# WEATHER: Check for combinations of year, month, day, hour, origin that are listed more than once in the weather table
weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n>1)
# There are only three out of >26,000 rows. Probably entry error.

# WEATHER: By contrast, plenty of planes leave at the same time from different places
weather %>%
  count(year, month, day, hour) %>%
  filter(n>1)
# >8,695

# FLIGHTS: Does flights have a primary key? Maybe a combination of date and flight number?
flights %>%
  count(year, month, day, flight) %>%
  filter(n>1)

# FLIGHTS: How about date and tail number?
flights %>%
  count(year, month, day, tailnum) %>%
  filter(n>1)

# FLIGHTS: What if I add scheduled departure time?
flights %>%
  #count(year, month, day, sched_dep_time, flight) %>% # Nope
  count(year, month, day, sched_dep_time, tailnum) %>% # Not this one, either
  filter(n>1)

# FLIGHTS: Create a surrogate key with mutate() and row_number()
(flights %>%
    mutate(num = row_number()))

##################
# Mutating Joins #
##################

# Match based on variable names, linking observations in one table to data in another. 
# The main use of mutating joins is to add information.

# Make a table with fewer variables so it's more manageable / easier to see
(flights2 <- flights %>%
   select(year:day, hour, origin, dest, tailnum, carrier))

# Add airline data with a mutating left-join
(flights2 %>%
    select(-origin, -dest) %>% # remove origin and destination because reasons?
    left_join(airlines, by = "carrier")) # add carrier information

# Doing the same thing with (partial) Base R is more complex
(flights2 %>%
    select(-origin, -dest) %>% # remove origin and destination because reasons?
    mutate(name = airlines$name[match(carrier, airlines$carrier)])) # Base R mutate matched on carrier

# Base R can also perform joins using merge(), but again, is more complex. It also results in a Base data frame, not a tibble.
View(merge(flights2, airlines, all.x=TRUE))

# If you don't specify a join-by key, it defaults to use variables with the same name that are present in both tables ("natural join").
(flights2 %>%
    left_join(weather))

# Define by a char vector; it wil only use the named variable specified. Useful if variables with the same name in different tables actually mean different things.
(flights2 %>%
    left_join(planes, by = "tailnum")) # year exists in both tables, but means different things. So, we specify that we only want to use tailnum

# Define by two differently named variables that store the same information
(flights2 %>%
    left_join(airports, c("dest" = "faa")))

###################
# Filtering Joins #
###################

# Match based on variable values, keeping (filtering for) only the matches (x -> y). 
# The main use of filtering joins is to filter the dataset down to a limited set.

# semi_join(x, y) # Keeps matching observations
# anti_join(x, y) # Drops matching observations, good for checking key integrity

# Find the top 10 most-visted destinations and store in a key table
(top10_dest <- flights %>%
   count(dest, sort = TRUE) %>%
   head(10))

# Filter the flights table by the top10 table using semi_join()
(flights %>%
    semi_join(top10_dest))

# It's possible to filter the flights table by the top10 table by hand, but this is more complex.
(flights %>%
    filter(dest %in% top10_dest$dest))

# See if there are any flights by planes not listed in the planes df
(flights %>%
    anti_join(planes, by = "tailnum") %>%
    count(tailnum, sort = TRUE))
# More than 2500 flights don't have a tailnum at all. Other flights by unlisted tailnums number in the hundreds.

##################
# Set Operations #
##################

# intersect(x, y) # return only observations in both x and y
# union(x, y) # return unique observations in x and y
# setdiff(x, y) # return observations in x but not y
# All work with observations across a complete row, instead of matching by a single variable

# Create two simple tables to experiment with.
df1 <- tribble(
  ~x, ~y,
  1,   1,
  2,   2
)
df2 <- tribble(
  ~x, ~y,
  1,   1,
  1,   2
)

intersect(df1, df2)
union(df1,df2)
setdiff(df1,df2)
setdiff(df2,df1)

##########################################################################

# Chapter 11: Strings with stringr

# Started Dec. 22, 2018
# By Roxanne Ready

# Load packages
#install.packages(c("tidyverse", "nycflights13", "gapminder", "Lahman", "stringr"))
#install.packages(c("htmlwidgets", "htmltools"))
library(tidyverse)
library(stringr)
library(htmlwidgets)
library(htmltools)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtn:        run code line/block
# cmd-opt-e:      run code until EoF

# \

##########################################

#################
# String basics #
#################

string1 <- "This is a string."
string2 <- 'Single quotes do the same thing and are good to use if you want to put acutal "quotation marks" in side your string.'

# Backslash \ is the escape character.
string2 <- "I could write a string with \"escaped quotation marks\" instead of using single quotes as above."
print(string2) # print() shows the escapes...
writeLines(string2) #...but writeLines() shows the content without them.

# \n newline
# \t tab
# \u00b5 and so on are character encodings for non-English characters
# ?"'" shows other special characters that can be used inside "s

# Store character vectors using c()
(vector1 <- c("a", "bcd", "c"))

# Combine strings using str_c()
# paste() does the same in Base R but is less robust
(vector2 <- str_c("a", "bcd", "c"))
str_c("prefix-", c("a", "b", "c"), "-suffix")

# Count
str_length(vector1) # the length of each of the strings stored in vector1
str_length(string2) # the total length of the string stored in string2

# Handling NA
x <- c("abc", NA)
str_c("|-", x, "-|") # NA is treated as a non-item and therefore cannot be manipulated
str_c("|-", str_replace_na(x), "-|") # NA is printed as the character string "NA"

# Used with if
name <- "John"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if(birthday) ", and happy birthday",
  "."
)

# Subsetting
x <- c("apple", "banana", "pear")
str_sub(x, 1, 3) # select characters from 1 to 3
str_sub(x, -3, -1) # select backwards from the last to the third-from-last

# Case changing - note that different capitalization rules can be set by locale, a 2-3 letter ISO-639 code
(str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))) # select the first characters and assign them uppercase values
x # show string values

# Base R has order() and sort(), but these default to the user's OS locale. 
# For better reproducibility, use str_sort(x, locale = "en").

##########################################
# Regular Expressions - Pattern Matching #
##########################################

# A string vector to play with
x <- c("apple", "banana", "pear")

# Match an exact character
str_view(x, "an")

# Match any character (except newline) using .
str_view(x, ".a.")

# To match the literal character . (instead of using . as a regex argument), it needs to be escaped with \
# But to keep R from escaping it out of the string, you need an extra \
# Therefore, the regex for matching the literal character . is \\.

dot <- "\\."                               # Assign the dot regex to a variable, to make re-use easier
writeLines(dot)                            # Check that R understands it correctly
str_view(c("abc", "a.c", "bef"), "a\\.c")  # Match on \\.
str_view(c("abc", "a.c", "bef"), dot)      # Match on variable dot
str_view(c("abc", "a.c", "bef"), str_c("a",dot,"c")) # Match using str_c and the variable dot

# To match \, a regex within R should look like \\\\ (4 \s)

# Match from the start of the string using ^
str_view(x, "^a") # match = apple
str_view(x, "a^") # meaningless, no match

# Match from the end of the string using $
str_view(x, "a$") # match = banana
str_view(x, "$a") # meaningless, no match

# Mneumonic: If you begin with power ^, you end with money $.

# Match beginning to end by sandwiching them together
x <- c("apple pie", "apple", "apple cake") # An apple string vector to play with
str_view(x, "apple") # Matches everything with apple
str_view(x, "^apple$") # Matches only the complete string

str_view(c("abc", "a.cjr", "df"), "^...$") # Match for length of 3

####################################
# Matching other character classes #
####################################

# \\. any character (note the extra \ because it's a regex within R)
# \\d any digit 
# \\s any whitespace (space, tab, newline)
# [abc] a, b, OR c
# [^abc] anything EXCEPT a, b, OR c

# Match for OR
str_view(c("grey", "gray"), "gr(e|a)y") # can use |
str_view(c("grey", "gray"), "gr[ea]y") # or []

#########################
# Controling Repetition #
#########################

# How many times does the pattern match?
# ? for 0 or 1
# * for 0 or more
# + for 1 or more
# {n} for exactly n
# {n,} for n or more
# {,m} for at most m
# {n,m} for between n and m

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVII."
str_view(x, "CC?") # Check that 0 or 1 match
str_view(x, "CC+") # Check that 1 or more match
str_view(x, "CC*") # Check that 0 or more match
str_view(x, "C[LX]+")
str_view(x, "C{2}")
str_view(x, "C{3}")
str_view(x, "C{1}")
str_view(x, "C{1,}")
str_view(x, "C{1,2}")

str_view(fruit, "(..)", match = TRUE) # Matches any (the first) two letters
str_view(fruit, "(..)\\1", match = TRUE) # Matches any pair of two letters that repeat
str_view(fruit, "(.)\\1", match = TRUE) # Matches any letter that repeats

#################
# stringr Tools #
#################

# Check if vector items match a pattern.
# Returns TRUE/FALSE (1/0) for each item in vector.
str_detect(fruit, "e$")

# Use 0/1 value of logicals to count matches
sum(str_detect(fruit, "^t")) # How many fruits start with t?
mean(str_detect(fruit, "[aeiou]$")) # What proportion of fruits end in a vowel?

# Two approaches to finding all matches for "doesn't contain vowels"
no_vowels_bad <- str_detect(words, "^[^aeiou]+$") # Find all words that only contain non-vowels.
no_vowels_good <- !str_detect(words, "[aeiou]") # Find all words that contain vowels, then don't include those words.
identical(no_vowels_bad, no_vowels_good) # Results are the same, but *_good is easier to read and write.
sum(!str_detect(words, "[aeiou]")) # There are six words in the list that don't contain vowels.

# Use str_detect to subset the data (pull out specific elements) from a vector
words[str_detect(words, "x$")] # Use logical subsetting to select words ending with x
str_subset(words, "x$") # Do the same using str_subset for simplicity

# Make a tibble to experiment with
(df <- tibble(
  words = words,
  i = seq_along(word)
))

# Use str_detect() with filter() on a df
df %>%
  filter(str_detect(words, "x$"))

# str_count() gives the number of matches in each, instead of a logical TRUE/FALSE
x <- c("apple", "banana", "pear")
str_count(x, "a") # Count how many a's are in each word
mean(str_count(words, "[aeiou]")) # Average of how many vowels there are per word (about 2)

# Use str_count() with mutate() to add count columns
(df %>%
    mutate(
      vowels = str_count(words, "[aeiou]"),
      consonants = str_count(words, "[^aeiou]")
    ))

# Looking at overlap
str_count("abababa", "aba") # 2, not 3 matches
str_view_all("abababa", "aba") # Illustrated by this output
str_view("abababa", "aba") # This only shows the first match

######################
# Extracting Matches #
######################

# A case study finding all sentences that include a color

# Explore the sentences df
length(sentences)
head(sentences)

# ATTEMPT 1
# Create a color vector to check against
(colors <- c("red", "orange", "yellow", "green", "blue", "purple"))

# Collapse that into a single string with the OR character separating them |
(color_match <- str_c(colors, collapse = "|"))

# Check for sentences that have a color
(has_color <- str_subset(sentences, color_match)) # Ha! These include "colors" that are part of other words.

# ATTEMPT 2
# Refine the colors check to only check for whole words (note this doesn't check for hyphenated colors or colors immediately preceding punctuation).
(colors <- c(" red ", " orange ", " yellow ", " green ", " blue ", " purple "))

# Again, collapse that into a single string with the OR character separating them |
(color_match <- str_c(colors, collapse = "|"))

# Check for sentences that have a color
(has_color <- str_subset(sentences, color_match)) # Better!

# Let's see what colors they are using str_extract()
(matches <- str_extract(has_color, color_match)) # These display dumb because they include the blank spaces. 

# ATTEMPT 3
# Will it do the same thing if I use \\s instead of a literal space?
(colors <- c("\\sred\\s", "\\sorange\\s", "\\syellow\\s", "\\sgreen\\s", "\\sblue\\s", "\\spurple\\s"))
(color_match <- str_c(colors, collapse = "|"))
(has_color <- str_subset(sentences, color_match))
(matches <- str_extract(has_color, color_match)) # Unfortunately, still shows the dumb spaces.
#(matches <- str_extract(matches, "[^\\s*]")) # There is a way to strip them out (this isn't it), but for now I'm going to move on with the lesson.

# Extract all matches with str_extract_all()
(matches <- str_extract(has_color, color_match)) # Shows the colors, but only pulls the first match in the sentence.
(more <- sentences[str_count(sentences, color_match) > 1]) # Store the sentences with more than one color.
str_extract(more, color_match) # Prove it doesn't show the second color.

str_extract_all(more, color_match) # Returns a list holding all matches for each sentence.
str_extract_all(more, color_match, simplify = TRUE) # Returns a matrix
head(str_extract_all(sentences, color_match, simplify = TRUE)) # Note that it gives a row for each, even those that don't match

###################
# Grouped Matches #
###################

# Define a noun (approximately) using groups of checks, parsed with ()
noun <- "(a|the) ([^ ]+)" 
# A sequence of at least one character that isn't a space, coming after an article (a/the). 
# This ignores nouns that might start a sentence, grouped nouns that don't need articles and proper nouns, and will include some adjectives and other false positives. But it's just a basic practice check.

# Find all sentences with nouns
(has_noun <- sentences %>%
    str_subset(noun) %>%
    head(10)) # Limit the number to 10 because really, most will have nouns...

# Pull out the matches
has_noun %>%
  str_extract(noun) # Result pulls the complete match and so includes the article
has_noun %>%
  str_match(noun) # Result generates a matrix. It puts the complete match in col1 and each group into its own col.

# TIBBLES
# Use tidyr::extract() instead
tibble(sentences_col = sentences) %>%
  extract(
    sentences_col, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )

#####################
# Replacing Matches #
#####################

# Vectors to play with
x <- c("apple", "pear", "banana")
y <- c("1 house", "2 cats", "3 people", "1 fish")

# Replace the first match
str_replace(x, "[aeiou]", "-")
x # note that it isn't stored anywhere, so x remains the same

# Replace all matches
str_replace_all(x, "[aeiou]", "-")

# Replace by name
str_replace_all(y, c("1" = "one", "3" = "three"))

# Manipulate matches with backreferences
sentences %>%
  str_replace(
    "([^ ]+) ([^ ]+) ([^ ]+)", # Each of these selects a word: Not a space and 1 or more characters.
    "\\1 \\3 \\2" # Each of these references one of the (components) defined above, swapping 2 and 3
  ) %>%
  head(5)

#############
# Splitting #
#############

# Split up the words by spliting on spaces
sentences %>%
  head(5) %>%
  #str_split(" ") # Split on space, output to a list.
  str_split(" ", simplify = TRUE) # Or set simplify to TRUE to output a matrix instead of a list. Note there will be as many columns as there are words in the longest sentence.

# To extract the first element of the list:
# ..from a length-1 vector
"a|b|c|d" %>%
  str_split("\\|") %>%
  .[[1]]

# ..from a complex df
sentences %>% 
  head(5) %>%
  str_split(" ") %>%
  .[[1]]

# Request a maximum number of pieces:
str_split(
  c("Name: Hadley", "Country: NZ", "Age: 35"),
  ": ",
  n = 2, # Only get the first 2, ignore Age
  simplify = TRUE
)

# Make a string of sentences to play with
(x <- str_c(sentences[1], " ", sentences[2]))

# Split on built-in patterns: character, word, sentence and line using boundary()
str_view_all(x, boundary("character"))
str_view_all(x, boundary("word"))
str_view_all(x, boundary("sentence"))
str_view_all(x, boundary("line"))

str_split(x, " ")[[1]] # This splits on a space.
str_split(x, boundary("word"))[[1]] # This splits on each word. Note that it understands and removes the punctuation.
str_to_lower(str_split(x, boundary("word"))[[1]]) # Normalize capitalization

# Find position values using str_locate() and str_locate_all()
str_locate(x, "th")
str_locate_all(x, "th")

####################
# Other regex args #
####################

# Explicitly call regex() to use its arguments.

# IGNORE_CASE
str_view_all(x, "the") # Case specific, so it only finds three matches.
str_view_all(x, regex("the", ignore_case = TRUE)) # Specify ignore_case to find all 4 instances. 
# Note that I could also use str_to_lower() on the dataset before running the check. Either might be better depending on circumstance.

# MULTILINE
# Create a sample to play with from my x vector that includes a new line.
x2 <- str_split(x, boundary("sentence"))[[1]]
writeLines(x3 <- str_c(x2[1], "\n", x2[2]))

str_extract_all(x3, "\\.$")[[1]] # Normally $ will check for only the end of the string.
str_extract_all(x3, regex("\\.", multiline = TRUE))[[1]] # Set multiline to make it check line by line instead.

# COMMENTS
phone <- regex("
               \\(?          # optional opening parens
               (\\d{3})      # 3 numbers (area code)
               [)\\-\\ \\.]? # optional closing parens, dash, space or period
               (\\d{3})      # another 3 numbers
               [\\ \\-\\.]?  # optional space, dash or period
               (\\d{3})      # three more numbers
               ", comments = TRUE) # Set comments to force regex to ignore all spaces and everything after #
str_match("514-791-8141", phone)

# FIXED: fixed() compares individual bytes and ignores regexrules. 
# It is faster than regex, but is very specific. Beware using it with non-English data. 
# p219.
head(str_detect(sentences, "the"))
head(str_detect(sentences, fixed("the")))

# COLL: coll() compares strings using human-style comparison rules. It is slower than regex or fixed.
# Useful for case-insensitive matching, because unlike regex and fixed, it takes a locale argument instead of relying on the system environment.
# p220

# See system invironment with stringi
stringi::stri_locale_info()

# OTHER REGEX USES
# Search the global environment with apropos().
apropos("^time")

# Search the system directory with dir().
head(dir(pattern = "\\.R$"))
head(dir(pattern = glob2rx("*.R"))) # glob2rx() allows familiar wildcards

# STRINGI
# stringr is bulit on stringi. stringi is more comprehensive, with 234 functions compared to stringr's 42. Use stringi if stringr isn't robust enough.
?stringi

library(stringi)

# Playing with random text

(x <- stringi::stri_rand_strings(1, 42)) # 1 set of random 42 characters

# generate n random passwords of length in [8, 14]
# consisting of at least one digit, small and big ASCII letter:
n <- 3
(stri_rand_shuffle(stri_paste(
  stri_rand_strings(n, 1, '[0-9]'),
  stri_rand_strings(n, 1, '[a-z]'),
  stri_rand_strings(n, 1, '[A-Z]'),
  stri_rand_strings(n, sample((n/2):(n+1), (n/2), replace=TRUE), '[a-zA-Z0-9]')
)))

# Generate 2 paragrapshs of lorem ipsum text.
stri_rand_lipsum(2, start_lipsum = TRUE)

##########################################################################

# Chapter 12: Factors with forcats

# Started Jan. 1, 2019
# By Roxanne Ready

# Load packages
#install.packages(c("tidyverse", "nycflights13", "gapminder", "Lahman", "stringr"))
#install.packages(c("htmlwidgets", "htmltools"))
library(tidyverse)
library(stringr)
library(htmlwidgets)
library(htmltools)
library(forcats)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtn:        run code line/block
# cmd-opt-e:      run code until EoF

# \

##########################################

##################
# Factors Basics #
##################

# Create list of valid levels, in order
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# Create variables holing months (one with invalid entries)
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "March")

# Store variable lists as factors
(y1 <- factor(x1, levels = month_levels))
(y2 <- factor(x2, levels = month_levels)) # Silently converts invalid strings to NA
(parse_factor(x2, levels = month_levels)) # View errors
levels(y2) # View factorial levels

# If unspecified, levels are alphabetical
(factor(x1))
levels(factor(x1))

# Choose the level order as the first appearance in the data
(factor(x1, levels = unique(x1))) # ...by using unique
f1 <- x1 %>%
  factor() %>%
  fct_inorder() # ...or by specifying it after the fact
f1

############################
# Working with sample data #
############################

gss_cat # A tibble holding 21,483 records of 9 variables

# View category levels within a tibble using count() or ggplot()
gss_cat %>%
  count(race)

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) # Force display of levels with empty values

# Create a summary table for religion data, including age, time spent watching TV, and number of respondents
(relig <- gss_cat %>%
    group_by(relig) %>%
    summarize(
      age = mean(age, na.rm = TRUE),
      tvhours = mean(tvhours, na.rm = TRUE),
      n = n()
    ))

# Plot the TV hours against religion (OK)
ggplot(relig, aes(tvhours, relig)) +
  geom_point()

# Use fct_reorder() to organize the religion factors by tvhours (BETTER)
ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

# Split fct_reorder() out from aes() using mutate() (BEST)
relig %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

# Look at ages across religions
relig %>%
  mutate(relig = fct_reorder(relig, age)) %>%
  ggplot(aes(age, relig)) +
  geom_point()

# Summary table holding income and age
(rincome <- gss_cat %>%
    group_by(rincome) %>%
    summarize(
      age = mean(age, na.rm = TRUE),
      tvhours = mean(tvhours, na.rm = TRUE),
      n = n()
    ))

# Plot
rincome %>%
  ggplot(aes(age, rincome)) +
  geom_point()

rincome %>%
  mutate(rincome = fct_reorder(rincome, age)) %>% # Reordering the income by the age is nonsensical
  ggplot(aes(age, rincome)) +
  geom_point()

rincome %>%
  mutate(rincome = fct_relevel(rincome, "Not applicable")) %>% # Instead, move NA values to the front
  ggplot(aes(age, rincome)) +
  geom_point()

# Summary table comparing age and marital status
(by_age <- gss_cat %>%
    filter(!is.na(age)) %>%
    group_by(age, marital) %>%
    count())
#mutate(prop = n / sum(n)) %>% ## This line failed because...
#mutate(prop_check = sum(n))) ## ...sum() only sums for that row.

# Plot the table
ggplot(by_age, aes(age, n, color = marital)) +
  geom_line(na.rm = TRUE)

by_age %>%
  mutate(by_age = fct_reorder2(marital, age, n)) %>%
  ggplot(aes(age, n, color = marital)) +
  geom_line() +
  labs(color = "marital")

gss_cat %>%
  mutate(marital = marital %>%
           fct_infreq() %>% # Order the factor "marital" by increasing frequency
           fct_rev()) %>% # Reverse it
  ggplot(aes(marital)) +
  geom_bar()


##########################
# Changing Factor Values #
##########################

gss_cat %>%
  count(partyid)

# Rename and collapse factors
# Will ignore things not explicitely named
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"       = "Strong republican",
                              "Republican, weak"         = "Not str republican",
                              "Independent, near rep"    = "Ind,near rep",
                              "Independent, near dem"    = "Ind,near dem",
                              "Democrat, weak"           = "Not str democrat",
                              "Democrat, strong"         = "Strong democrat",
                              "Other"                    = "No answer", # Collapse levels by giving same name
                              "Other"                    = "Don't know",
                              "Other"                    = "Other party"
  )
  ) %>%
  count(partyid)

# Collapse many factors at once
gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )
  ) %>%
  count(partyid)

# Lump small values together into one
gss_cat %>%
  mutate(partyid = fct_lump(partyid)) %>%
  count(partyid)

# By default, lumps progressively so the aggregate is still the smallest group
gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig) # Overcollapsed

# Specify how many groups to collapse to
gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE)

##########################################################################

# Chapter 13: Dates and Times with lubridate

# Started Jan. 3, 2019
# By Roxanne Ready

# Load packages
#install.packages(c("tidyverse", "nycflights13", "gapminder", "Lahman", "stringr"))
#install.packages(c("htmlwidgets", "htmltools"))
library(tidyverse)
library(lubridate)
library(nycflights13)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtrn:       run code line/block
# cmd-opt-e:      run code until EoF

# \

##########################################

# Date : <date>
# Time : <time> (hms package)
# Date-Time : <dttm> or POISXct

today() # Date
now() # dttm

# parse_*(),from Ch. 8, p. 134
parse_datetime("2010-10-01T2010")
parse_date("2010-10-01")
parse_date("2010/10/01")
parse_date("01/02/10", "%m/%d/%y")
parse_time("01:10 am")

# lubridate functions: reorder letters according to the data being parsed
ymd("2017-01-31") 
ymd(20170131) 
mdy("January 31st, 2017")
dmy("31-January-2017")

# can add _hms() for hour/min/second, _hm() for hour/min, etc, or by adding a time zone
ymd_hms("2017-01-31 20:11:59")
ymd(20170131, tz = "UTC")

# From individual components, use make_date() or make_datetime()
flights %>%
  select(year, month, day, hour, minute) %>%
  mutate(
    departure = make_datetime(year, month, day, hour, minute)
  )

# Create a function to properly parse the time data
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

# Store parsed date/time info using that function
flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

# Visualize distribution of departure times across the year
flights_dt %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

# Visualise same for one day
flights_dt %>%
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

# Swapping between dttm and date
as_datetime(today())
as_date(now())

# For Unix Epoch seconds, see p. 242

# Getting components
datetime <- ymd_hms("2016-07-08 12:34:56") # variable to test
year(datetime)
month(datetime)
month(datetime, label = TRUE, abbr = FALSE) # month, by name (full)
mday(datetime) # day of month
yday(datetime) # day of year
wday(datetime) # day of week, by number
wday(datetime, label = TRUE, abbr = TRUE) # day of week, by name (abbreviated)

flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE))  %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

# Rounding Dates 
flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

# Setting date components
(datetime <- ymd_hms("2016-07-08 12:34:56")) # variable to play with

# One component at a time
year(datetime) <- 2020
datetime
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime

# Multiple components at once
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

# Set all days equal to explore aggregated information
flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% # With all days set to Jan 1, the data are collapsed
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

##############
# Time Spans #
##############

# durations: an exact number of seconds.
# periods: human units like weeks and months.
# intervals: a starting and ending point.

# DURATIONS
# Strict durations

(h_age <- today() - ymd(19851102))
as.duration(h_age)

# Duration info functions
dseconds(60)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(5)
dyears(1)

(tomorrow <- today() + ddays(1))

# Auto-converts time zones but always adds 24 hours, which may lead to unexpected results
(one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York"))
one_pm + ddays(1) # Note the time change from EST to EDT, changing the hour

# PERIODS
# Intuit the human interpretation of a day instead of strict 24 hours
# Better for arithmetic

one_pm +days(1) # Works as expected

seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)

flights_dt %>% 
  filter(overnight, arr_time < dep_time) # Shows flights arriving before they departed

# Fix overnight flights so they arrive after they depart
flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )
flights_dt %>% 
  filter(overnight, arr_time < dep_time) # Proves overnight flights now obey laws of physics

# INTERVALS
# Fix some problems with leap years, etc.
(next_year <- today() + years(1))
(today() %--% next_year / ddays(1))
(today() %--% next_year) %/% days(1)

##############
# Time Zones #
##############

# Uses IANA time zones, internationally compliant format in <continent>/<city> format
# eg. "America/New_York" and "Europe/Paris"

Sys.timezone()
OlsonNames()

# lubridate defaults to UTC
# GMT was UTC's predecessor

# These are all equivalent
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
x1 - x2 # verified same instant in time

(x4 <- c(x1, x2, x3)) # Converts to local time zone

# Change how the time is displayed, but not the instant in time
(x4a <- with_tz(x4, tzone = "Australia/Lord_Howe"))
x4a - x4

# Change the instant in time because it was coded with an incorrect time zone
(x4b <- force_tz(x4, tzone = "Australia/Lord_Howe"))
x4b - x4

##########################################################################

# Chapters 15 (Functions), 16 (Vectors), 17 (Itteration with purr)

# Started Jan. 8, 2019
# By Roxanne Ready

# Load packages
#install.packages(c("magrittr"))
#library(magrittr)
library(tidyverse)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtrn:       run code line/block
# cmd-opt-e:      run code until EoF
# cmd-shft-r:     insert section break

# \

# Test section 0 ##########################################

?'if'
x <- 1L # The L specifies that this is an integer, not a double
y <- 0
z <- 1 # Numbers are doubles by default
# || or
# && and
identical(x, z) # integers, doubles, and floating points will not be coerced
near(x, z)
all(c(x == y, x == x)) # true for all in a list of comparisons
any(c(x == y, x == x)) # true for any "   "

# Test section 1 ------------------------------------------------------------

if (this) {
  # do thing 1
} else if (that) {
  # do thing 2
} else {
  # do thing 3
}

maths <- function(x, y, operator) {
  switch(operator,
         plus = x + y,
         minus = x - y,
         times = x * y,
         multiply = x * y, # how to collapse into one?
         # (times || multiply) = x * y, # Doesn't work
         divide = x / y,
         stop("Unknown operator")
  )
}

maths(1, 5, "times")
maths(1, 5, "remainder")

?'if'
?ifelse

# Test section 2 ------------------------------------------------------------

?stop # Stop the function with an error message

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

wt_mean(1:6, 1:3)

?stopifnot # Stop the function if the value is not true. Faster, but the error message is not as detailed.

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1) # Stop if not a valid input for na.rm
  stopifnot(length(x) == length(w)) # Stop if the lengths aren't equal
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")

# ...
commas <- function(...) {stringr::str_c(..., collapse = ", ")}
commas(letters[1:10])


# Bad
f <- function() {
  if (x) {
    # Do 
    # something
    # that
    # takes
    # many
    # lines
    # to
    # express
  } else {
    # return something short
  }
}

# Good
f <- function() {
  if (!x) {
    return(something_short)
  }
  
  # Do 
  # something
  # that
  # takes
  # many
  # lines
  # to
  # express
}


# Vectors -----------------------------------------------------------------

typeof(letters)
typeof(1:10)
length(letters)

# Checks for doubles
# is.finite()
# is.infinite()
# is.na()
# is.nan()

# Checks for types
# Use purr (is_*) not baser (is.*)
# is_logical
# is_integer
# is_double
# is_numeric
# is_character
# is_atomic
# is_list
# is_vector

x <- list("a", "b", "c")
x
str(x)

x_named <- list(cola = "a", colb = "b", colc = "c")
x_named
str(x_named)

x <- set_names(x, c("ColA", "ColB", "ColC"))
str(x)

listception <- list("firstList" = x, "secondList" = x_named)
str(listception)


# Itteration --------------------------------------------------------------

# Test df
df <- tibble(
  a = rnorm(10), # Random normal generation
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", ncol(df)) # Set the output size
for (i in seq_along(df)) { # Run a for loop
  output[[i]] <- median(df[[i]]) # Replace the output at each position with the median of the corresponding df col
}
output # View the output

# Exercises
#1a. Compute the mean of every column in mtcars.
mtcars
output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[[i]])
}
output

#1b. Determine the type of each column in nycflights13::flights.
nycflights13::flights # Look at flights
sapply(nycflights13::flights[1], class) # Check how to find the col type
output <- vector("character", ncol(nycflights13::flights)) # Set the output length
for (i in seq_along(nycflights13::flights)) {
  output[[i]] <- sapply(nycflights13::flights[i], class)
}
output

#1c. Compute the number of unique values in each column of iris.
iris # Look at iris
summary(iris)

# Check how to find and count unique values in a col
?unique
count(unique(iris[1])) 

output <- vector("integer", ncol(iris)) # set the output length
for (i in seq_along(iris)) {
  output[[i]] <- dplyr::pull( # Un-nest, or un-tibble the tibble that count() creates
    count( # Count how many
      unique(iris[i]))) # Find uniques
}
output

#1d. Generate 10 random normals for each of -10, 0, 10, 100
# Generate a single random normal
?rnorm
(rnorm(1, mean = -10))

(output <- vector("double", 4)) # Set output length
(mu <- -10)
for (i in seq_along(output)) {
  if (i == 1) { # On first itteration...
    # ...exit for loop
  } else if (i == 4) { # On last itteration...
    mu <- mu * 10 # ... multiply mu by 10
  } else { # On all other itterations...
    mu <- mu + 10 # ... add 10 to mu
  }
  output[i] <- rnorm(1, mu) # Store an RNG num with a mean of mu in output
}
output # View output


# For loop variations -----------------------------------------------------

# Test df
df <- tibble(
  a = rnorm(10), # Random normal generation
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Function to rescale
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

# Itteratively deploy function to the df
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}
df
df[1]
df[[1]]

# Understanding lists in for loops
output <- list(vector("character", ncol(iris)), vector("character", ncol(iris)))
for (i in seq_along(iris)) {
  name <- names(iris)[[i]] # Extract the name of a column
  #value <- iris[[i]] # Extract the value of a variable
  mean <- mean(iris[[i]][[i]]) # Extract the mean of a column
  output[[1]][[i]] <- name
  output[[2]][[i]] <- mean
}
output
output[[2]][[3]]

# Naming outputs
output <- vector("list", length(iris))
for (i in seq_along(iris)) {
  names(output) <- stringr::str_c("Mean_", names(iris))
  output[[i]] <- mean(iris[[i]])
}
output
output$Petal.Length

# Unknown output length
means <- c(0, 1, 2) # A vector of 3 numbers
out <- vector("list", length(means)) # A vector of lists of the same length as "means"
?sample
?rnorm
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
out # A vector of 3 lists, each holding a random number of numbers
str(out) # Shows the structure of "out"
out2 <- unlist(out) # Flatten those three lists into one vector
out2 # A vector of numbers
str(unlist(out)) # Shows the structure of out2

# Other useful functions to store iterations and optimize for loops
paste(output, collapse = "") # To combine character strings saved in a vector
dplyr::bind_rows(output) # To combine rows


# While loops -------------------------------------------------------------

## Find how many flips it takes to get three heads in a row

# Flip function
flip <- function() {
  sample(c("T", "H"), 1) # Pull one value as T or H 
}

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0 # Reset heads to 0
  }
  flips <- flips + 1
}
flips


# Functionals -------------------------------------------------------------

# Test df
df <- tibble(
  a = rnorm(10), # Random normal generation
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Use an argument of a function as a call to another
col_summary <- function(df, fun) { # Note fun here
  out <- vector("double", length(df))
  for(i in seq_along(df)) {
    out[i] <- fun(df[[i]]) # Fun now becomes fun() acting with respect to df and i
  }
  out
}
col_summary(df, median)
col_summary(df, mean)
col_summary(df, sum)


# purr functionals --------------------------------------------------------

# map() makes a list.
# map_lgl() makes a logical vector.
# map_int() makes an integer vector.
# map_dbl() makes a double vector.
# map_chr() makes a character vector.

# The main benefit to purr's map functions is clarity, not speed.
# For loops aren't any slower and haven't been for many years.

# Same as the homebrew function written above (line 335-344)
map_dbl(df, median)
map_dbl(df, mean)
map_dbl(df, sum)

df %>% map_dbl(median)

?map_dbl

# Split mtcars into values along cylinders
models <- mtcars %>%
  split(.$cyl) %>% # Run the split; still has all info
  #map(function(df) lm(mpg ~ wt, data = df)) # Use an anonymous function to do the below; verbose
  map(~lm(mpg ~ wt, data = .)) # Replace info with a summary built from an anonymous function, using shortcuts

# Extract a summary statistic
models %>%
  map(summary) %>%
  #map_dbl(~.$r.squared) # Using expected syntax
  map_dbl("r.squared") # Using a shortcut string

# Select elements within a nested list by position
(x <- list(list(1,2,3), list(4,5,6), list(7,8,9))) # List of lists to play with
x %>%
  map_dbl(2)


# Handling Mapping Errors -------------------------------------------------

# Safely mapping functions so one error doesn't obfuscate all results
x <- list(1, 10, "a")
y <- x %>%
  map(safely(log))
y
str(y)

# Transpose to put all results in one list and all errors in another
y <- y %>%
  transpose()
str(y)
y

# Use the errors to pull out usable info
is_ok <- y$error %>%
  map_lgl(is_null)
str(is_ok)

x[!is_ok] # View values of x where y is an error

y$result[is_ok] %>% # View the y values that are not errors
  flatten_dbl()

# possibly() is a simpler safely(), outputting a default error return instead of error messages
x %>%
  map_dbl(possibly(log, NA_real_))


# Mapping over multiple arguments -----------------------------------------

# Use map2() and pmap(), pp. 332-335
# Use walk() to handle printouts and file saves pp. 335-336

# reduce(dfs, full_join) will combine two dfs in a list into one, joined on a common element
# reduce(vs, inersect) will reduce two vectors in a list into their intersection


##########################################################################
