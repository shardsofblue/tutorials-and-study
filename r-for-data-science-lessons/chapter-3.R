# Code lessons from: 
# R for Data Science
# by Hadley Wickham & Garrett Grolemund

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
