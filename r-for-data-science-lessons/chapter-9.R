# Code lessons from: 
# R for Data Science
# by Hadley Wickham & Garrett Grolemund

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


