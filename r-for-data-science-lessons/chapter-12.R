# Code lessons from: 
# R for Data Science
# by Hadley Wickham & Garrett Grolemund

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

