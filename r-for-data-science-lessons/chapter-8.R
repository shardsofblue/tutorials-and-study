# Code lessons from: 
# R for Data Science
# by Hadley Wickham & Garrett Grolemund

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





