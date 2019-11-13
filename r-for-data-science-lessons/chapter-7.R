# Code lessons from: 
# R for Data Science
# by Hadley Wickham & Garrett Grolemund

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
