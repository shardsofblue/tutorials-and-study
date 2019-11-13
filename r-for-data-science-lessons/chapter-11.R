# Code lessons from: 
# R for Data Science
# by Hadley Wickham & Garrett Grolemund

# Chapter 11: Strings with stringr

# Started Dec. 22, 2018
# By Roxanne Ready

# Load packages
#install.packages(c("tidyverse", "nycflights13", "gapminder", "Lahman", "stringr"))
#install.packages(c("htmlwidgets", "htmltools"))
library(tidyverse)
library(stringr)
library(htmlwidgets)
library(htmltools)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtn:        run code line/block
# cmd-opt-e:      run code until EoF

# \

##########################################

#################
# String basics #
#################

string1 <- "This is a string."
string2 <- 'Single quotes do the same thing and are good to use if you want to put acutal "quotation marks" in side your string.'

# Backslash \ is the escape character.
string2 <- "I could write a string with \"escaped quotation marks\" instead of using single quotes as above."
print(string2) # print() shows the escapes...
writeLines(string2) #...but writeLines() shows the content without them.

# \n newline
# \t tab
# \u00b5 and so on are character encodings for non-English characters
# ?"'" shows other special characters that can be used inside "s

# Store character vectors using c()
(vector1 <- c("a", "bcd", "c"))

# Combine strings using str_c()
# paste() does the same in Base R but is less robust
(vector2 <- str_c("a", "bcd", "c"))
str_c("prefix-", c("a", "b", "c"), "-suffix")

# Count
str_length(vector1) # the length of each of the strings stored in vector1
str_length(string2) # the total length of the string stored in string2

# Handling NA
x <- c("abc", NA)
str_c("|-", x, "-|") # NA is treated as a non-item and therefore cannot be manipulated
str_c("|-", str_replace_na(x), "-|") # NA is printed as the character string "NA"

# Used with if
name <- "John"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if(birthday) ", and happy birthday",
  "."
)

# Subsetting
x <- c("apple", "banana", "pear")
str_sub(x, 1, 3) # select characters from 1 to 3
str_sub(x, -3, -1) # select backwards from the last to the third-from-last

# Case changing - note that different capitalization rules can be set by locale, a 2-3 letter ISO-639 code
(str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))) # select the first characters and assign them uppercase values
x # show string values

# Base R has order() and sort(), but these default to the user's OS locale. 
# For better reproducibility, use str_sort(x, locale = "en").

##########################################
# Regular Expressions - Pattern Matching #
##########################################

# A string vector to play with
x <- c("apple", "banana", "pear")

# Match an exact character
str_view(x, "an")

# Match any character (except newline) using .
str_view(x, ".a.")

# To match the literal character . (instead of using . as a regex argument), it needs to be escaped with \
# But to keep R from escaping it out of the string, you need an extra \
# Therefore, the regex for matching the literal character . is \\.

dot <- "\\."                               # Assign the dot regex to a variable, to make re-use easier
writeLines(dot)                            # Check that R understands it correctly
str_view(c("abc", "a.c", "bef"), "a\\.c")  # Match on \\.
str_view(c("abc", "a.c", "bef"), dot)      # Match on variable dot
str_view(c("abc", "a.c", "bef"), str_c("a",dot,"c")) # Match using str_c and the variable dot

# To match \, a regex within R should look like \\\\ (4 \s)

# Match from the start of the string using ^
str_view(x, "^a") # match = apple
str_view(x, "a^") # meaningless, no match

# Match from the end of the string using $
str_view(x, "a$") # match = banana
str_view(x, "$a") # meaningless, no match

# Mneumonic: If you begin with power ^, you end with money $.

# Match beginning to end by sandwiching them together
x <- c("apple pie", "apple", "apple cake") # An apple string vector to play with
str_view(x, "apple") # Matches everything with apple
str_view(x, "^apple$") # Matches only the complete string

str_view(c("abc", "a.cjr", "df"), "^...$") # Match for length of 3

####################################
# Matching other character classes #
####################################

# \\. any character (note the extra \ because it's a regex within R)
# \\d any digit 
# \\s any whitespace (space, tab, newline)
# [abc] a, b, OR c
# [^abc] anything EXCEPT a, b, OR c

# Match for OR
str_view(c("grey", "gray"), "gr(e|a)y") # can use |
str_view(c("grey", "gray"), "gr[ea]y") # or []

#########################
# Controling Repetition #
#########################

# How many times does the pattern match?
# ? for 0 or 1
# * for 0 or more
# + for 1 or more
# {n} for exactly n
# {n,} for n or more
# {,m} for at most m
# {n,m} for between n and m

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVII."
str_view(x, "CC?") # Check that 0 or 1 match
str_view(x, "CC+") # Check that 1 or more match
str_view(x, "CC*") # Check that 0 or more match
str_view(x, "C[LX]+")
str_view(x, "C{2}")
str_view(x, "C{3}")
str_view(x, "C{1}")
str_view(x, "C{1,}")
str_view(x, "C{1,2}")

str_view(fruit, "(..)", match = TRUE) # Matches any (the first) two letters
str_view(fruit, "(..)\\1", match = TRUE) # Matches any pair of two letters that repeat
str_view(fruit, "(.)\\1", match = TRUE) # Matches any letter that repeats

#################
# stringr Tools #
#################

# Check if vector items match a pattern.
# Returns TRUE/FALSE (1/0) for each item in vector.
str_detect(fruit, "e$")

# Use 0/1 value of logicals to count matches
sum(str_detect(fruit, "^t")) # How many fruits start with t?
mean(str_detect(fruit, "[aeiou]$")) # What proportion of fruits end in a vowel?

# Two approaches to finding all matches for "doesn't contain vowels"
no_vowels_bad <- str_detect(words, "^[^aeiou]+$") # Find all words that only contain non-vowels.
no_vowels_good <- !str_detect(words, "[aeiou]") # Find all words that contain vowels, then don't include those words.
identical(no_vowels_bad, no_vowels_good) # Results are the same, but *_good is easier to read and write.
sum(!str_detect(words, "[aeiou]")) # There are six words in the list that don't contain vowels.

# Use str_detect to subset the data (pull out specific elements) from a vector
words[str_detect(words, "x$")] # Use logical subsetting to select words ending with x
str_subset(words, "x$") # Do the same using str_subset for simplicity

# Make a tibble to experiment with
(df <- tibble(
  words = words,
  i = seq_along(word)
))

# Use str_detect() with filter() on a df
df %>%
  filter(str_detect(words, "x$"))

# str_count() gives the number of matches in each, instead of a logical TRUE/FALSE
x <- c("apple", "banana", "pear")
str_count(x, "a") # Count how many a's are in each word
mean(str_count(words, "[aeiou]")) # Average of how many vowels there are per word (about 2)

# Use str_count() with mutate() to add count columns
(df %>%
  mutate(
    vowels = str_count(words, "[aeiou]"),
    consonants = str_count(words, "[^aeiou]")
  ))

# Looking at overlap
str_count("abababa", "aba") # 2, not 3 matches
str_view_all("abababa", "aba") # Illustrated by this output
str_view("abababa", "aba") # This only shows the first match

######################
# Extracting Matches #
######################

# A case study finding all sentences that include a color

# Explore the sentences df
length(sentences)
head(sentences)

# ATTEMPT 1
# Create a color vector to check against
(colors <- c("red", "orange", "yellow", "green", "blue", "purple"))

# Collapse that into a single string with the OR character separating them |
(color_match <- str_c(colors, collapse = "|"))

# Check for sentences that have a color
(has_color <- str_subset(sentences, color_match)) # Ha! These include "colors" that are part of other words.

# ATTEMPT 2
# Refine the colors check to only check for whole words (note this doesn't check for hyphenated colors or colors immediately preceding punctuation).
(colors <- c(" red ", " orange ", " yellow ", " green ", " blue ", " purple "))

# Again, collapse that into a single string with the OR character separating them |
(color_match <- str_c(colors, collapse = "|"))

# Check for sentences that have a color
(has_color <- str_subset(sentences, color_match)) # Better!

# Let's see what colors they are using str_extract()
(matches <- str_extract(has_color, color_match)) # These display dumb because they include the blank spaces. 

# ATTEMPT 3
# Will it do the same thing if I use \\s instead of a literal space?
(colors <- c("\\sred\\s", "\\sorange\\s", "\\syellow\\s", "\\sgreen\\s", "\\sblue\\s", "\\spurple\\s"))
(color_match <- str_c(colors, collapse = "|"))
(has_color <- str_subset(sentences, color_match))
(matches <- str_extract(has_color, color_match)) # Unfortunately, still shows the dumb spaces.
#(matches <- str_extract(matches, "[^\\s*]")) # There is a way to strip them out (this isn't it), but for now I'm going to move on with the lesson.

# Extract all matches with str_extract_all()
(matches <- str_extract(has_color, color_match)) # Shows the colors, but only pulls the first match in the sentence.
(more <- sentences[str_count(sentences, color_match) > 1]) # Store the sentences with more than one color.
str_extract(more, color_match) # Prove it doesn't show the second color.

str_extract_all(more, color_match) # Returns a list holding all matches for each sentence.
str_extract_all(more, color_match, simplify = TRUE) # Returns a matrix
head(str_extract_all(sentences, color_match, simplify = TRUE)) # Note that it gives a row for each, even those that don't match

###################
# Grouped Matches #
###################

# Define a noun (approximately) using groups of checks, parsed with ()
noun <- "(a|the) ([^ ]+)" 
# A sequence of at least one character that isn't a space, coming after an article (a/the). 
# This ignores nouns that might start a sentence, grouped nouns that don't need articles and proper nouns, and will include some adjectives and other false positives. But it's just a basic practice check.

# Find all sentences with nouns
(has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)) # Limit the number to 10 because really, most will have nouns...

# Pull out the matches
has_noun %>%
  str_extract(noun) # Result pulls the complete match and so includes the article
has_noun %>%
  str_match(noun) # Result generates a matrix. It puts the complete match in col1 and each group into its own col.

# TIBBLES
# Use tidyr::extract() instead
tibble(sentences_col = sentences) %>%
  extract(
    sentences_col, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )

#####################
# Replacing Matches #
#####################

# Vectors to play with
x <- c("apple", "pear", "banana")
y <- c("1 house", "2 cats", "3 people", "1 fish")

# Replace the first match
str_replace(x, "[aeiou]", "-")
x # note that it isn't stored anywhere, so x remains the same

# Replace all matches
str_replace_all(x, "[aeiou]", "-")

# Replace by name
str_replace_all(y, c("1" = "one", "3" = "three"))

# Manipulate matches with backreferences
sentences %>%
  str_replace(
    "([^ ]+) ([^ ]+) ([^ ]+)", # Each of these selects a word: Not a space and 1 or more characters.
    "\\1 \\3 \\2" # Each of these references one of the (components) defined above, swapping 2 and 3
  ) %>%
  head(5)

#############
# Splitting #
#############

# Split up the words by spliting on spaces
sentences %>%
  head(5) %>%
  #str_split(" ") # Split on space, output to a list.
  str_split(" ", simplify = TRUE) # Or set simplify to TRUE to output a matrix instead of a list. Note there will be as many columns as there are words in the longest sentence.

# To extract the first element of the list:
# ..from a length-1 vector
"a|b|c|d" %>%
  str_split("\\|") %>%
  .[[1]]

# ..from a complex df
sentences %>% 
  head(5) %>%
  str_split(" ") %>%
  .[[1]]

# Request a maximum number of pieces:
str_split(
  c("Name: Hadley", "Country: NZ", "Age: 35"),
  ": ",
  n = 2, # Only get the first 2, ignore Age
  simplify = TRUE
)

# Make a string of sentences to play with
(x <- str_c(sentences[1], " ", sentences[2]))

# Split on built-in patterns: character, word, sentence and line using boundary()
str_view_all(x, boundary("character"))
str_view_all(x, boundary("word"))
str_view_all(x, boundary("sentence"))
str_view_all(x, boundary("line"))

str_split(x, " ")[[1]] # This splits on a space.
str_split(x, boundary("word"))[[1]] # This splits on each word. Note that it understands and removes the punctuation.
str_to_lower(str_split(x, boundary("word"))[[1]]) # Normalize capitalization

# Find position values using str_locate() and str_locate_all()
str_locate(x, "th")
str_locate_all(x, "th")

####################
# Other regex args #
####################

# Explicitly call regex() to use its arguments.

# IGNORE_CASE
str_view_all(x, "the") # Case specific, so it only finds three matches.
str_view_all(x, regex("the", ignore_case = TRUE)) # Specify ignore_case to find all 4 instances. 
# Note that I could also use str_to_lower() on the dataset before running the check. Either might be better depending on circumstance.

# MULTILINE
# Create a sample to play with from my x vector that includes a new line.
x2 <- str_split(x, boundary("sentence"))[[1]]
writeLines(x3 <- str_c(x2[1], "\n", x2[2]))

str_extract_all(x3, "\\.$")[[1]] # Normally $ will check for only the end of the string.
str_extract_all(x3, regex("\\.", multiline = TRUE))[[1]] # Set multiline to make it check line by line instead.

# COMMENTS
phone <- regex("
              \\(?          # optional opening parens
              (\\d{3})      # 3 numbers (area code)
              [)\\-\\ \\.]? # optional closing parens, dash, space or period
              (\\d{3})      # another 3 numbers
              [\\ \\-\\.]?  # optional space, dash or period
              (\\d{3})      # three more numbers
               ", comments = TRUE) # Set comments to force regex to ignore all spaces and everything after #
str_match("514-791-8141", phone)

# FIXED: fixed() compares individual bytes and ignores regexrules. 
# It is faster than regex, but is very specific. Beware using it with non-English data. 
# p219.
head(str_detect(sentences, "the"))
head(str_detect(sentences, fixed("the")))

# COLL: coll() compares strings using human-style comparison rules. It is slower than regex or fixed.
# Useful for case-insensitive matching, because unlike regex and fixed, it takes a locale argument instead of relying on the system environment.
# p220

# See system invironment with stringi
stringi::stri_locale_info()

# OTHER REGEX USES
# Search the global environment with apropos().
apropos("^time")

# Search the system directory with dir().
head(dir(pattern = "\\.R$"))
head(dir(pattern = glob2rx("*.R"))) # glob2rx() allows familiar wildcards

# STRINGI
# stringr is bulit on stringi. stringi is more comprehensive, with 234 functions compared to stringr's 42. Use stringi if stringr isn't robust enough.
?stringi

library(stringi)

# Playing with random text

(x <- stringi::stri_rand_strings(1, 42)) # 1 set of random 42 characters

# generate n random passwords of length in [8, 14]
# consisting of at least one digit, small and big ASCII letter:
n <- 3
(stri_rand_shuffle(stri_paste(
  stri_rand_strings(n, 1, '[0-9]'),
  stri_rand_strings(n, 1, '[a-z]'),
  stri_rand_strings(n, 1, '[A-Z]'),
  stri_rand_strings(n, sample((n/2):(n+1), (n/2), replace=TRUE), '[a-zA-Z0-9]')
)))

# Generate 2 paragrapshs of lorem ipsum text.
stri_rand_lipsum(2, start_lipsum = TRUE)

