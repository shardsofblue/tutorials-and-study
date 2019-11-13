# Code lessons from: 
# R for Data Science
# By Hadley Wickham & Garrett Grolemund

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

