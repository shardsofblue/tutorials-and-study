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
