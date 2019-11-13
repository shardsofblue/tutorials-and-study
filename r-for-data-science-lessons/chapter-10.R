# Code lessons from: 
# R for Data Science
# by Hadley Wickham & Garrett Grolemund

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
