# Code lessons from: 
# R for Data Science
# by Hadley Wickham & Garrett Grolemund

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



