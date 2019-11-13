# Code lessons from: 
# R for Data Science
# by Hadley Wickham & Garrett Grolemund

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
