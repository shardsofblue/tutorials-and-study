# Code lessons from: 
# Text Mining with R: A Tidy Approach
# By Julia Silge & David Robinson

# Header ---------------------------------------------------------------

# Started Jan. 17, 2019
# By Roxanne Ready

# Load packages
#install.packages(c("tidytext", "gutenbergr", "wordcloud", "viridis", "igraph", "ggraph", "widyr", "tm", "topicmodels", "broom"))
#install.packages("topicmodels")
library(tidyverse)
library(dplyr)
library(tidytext)
library(stringr)
library(janeaustenr)
library(ggplot2)
library(gutenbergr)
library(tidyr)
library(scales)
library(wordcloud)
library(viridis)
library(igraph)
library(ggraph)
library(widyr)
library(tm)
library(broom)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtrn:       run code line/block
# cmd-opt-e:      run code until EoF
# cmd-shft-r:     insert section break

# \

#
# Chapter 1: The Tidy Text Format ###############################################################
#

# Tidy text basics ---------------------------------------------------------------

# Tokenization: Splitting text into tokens (a single word, n-gram, sentence, or paragraph)

# Create a character vector from an Emily Dickinson poem
(dickinson <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The arriage held but just Ourselves -",
          "and Immortality"))

# Store it in a data frame (tibble)
(text_df <- data_frame(line = 1:4, text = dickinson))

# Break the text into tokens and transform it to a tidy data structure
text_df %>%
  unnest_tokens(word, text)

?unnest_tokens
# Can split by "words" (default), "characters", "character_shingles", "ngrams", "skip_ngrams", "sentences", "lines", "paragraphs", "regex", "tweets" (tokenization by word that preserves usernames, hashtags, and URLS ), and "ptb" (Penn Treebank)
# Can output as "text", "man", "latex", "html", or "xml"

# Tidying the works of Jane Austen ---------------------------------------------------------------

# Explore the original df
str(austen_books())
View(austen_books())
sapply(austen_books(), levels)

# Store the line number and chapter in a new col 
(original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup())
View(original_books)

# Restructure the df with tokenization
(tidy_books <- original_books %>%
  unnest_tokens(word, text))
View(tidy_books)

# Load in df of stop words ("of", "the", "to", etc.)
data(stop_words)

# Remove stop words from primary df
(tidy_books <- tidy_books %>%
  anti_join(stop_words))
# Can use filter() to use one set of stop words instead of all of them

# Count the most common words
tidy_books %>%
  count(word, sort = TRUE)

# Plot the most common words
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

# My own tinkering --------------------------------------------------

# Remove proper names from the analysis

# Store the names of characters by book
emma_names_v <- c("emma", "bates", "miss", "woodhouse", "philip", "elton", "george", "knightley")
mansfield_names_v <- c("edmund", "bertram", "maria", "thomas", "henry", "crawford", "mary", "rushworth", "fanny", "price")
northanger_names_v <- c("tilney", "catherine", "morland")
persuasion_names_v <- c("anne", "elliot", "frederick", "wentworth")
pride_names_v <- c("elizabeth", "bennet", "william", "collins", "darcy", "lady", "de", "bourgh", "wickham")
sense_names_v <- c("colonel", "brandon", "elinor", "dashwood", "marianne", "edward", "ferrars", "john", "willoughby")
all_names_v <- c("jane", "austen", "chapter")

# Create a df holding proper names in the books
(char_names <- data_frame(word = emma_names_v, book_name = "Emma"))

(char_names <- char_names %>%
  bind_rows(data_frame(word = mansfield_names_v, book_name = "Mansfield Park")) %>%
  bind_rows(data_frame(word = northanger_names_v, book_name = "Northanger Abbey")) %>%
  bind_rows(data_frame(word = persuasion_names_v, book_name = "Persuasion")) %>%
  bind_rows(data_frame(word = pride_names_v, book_name = "Pride & Prejudice")) %>%
  bind_rows(data_frame(word = sense_names_v, book_name = "Sense & Sensibility")) %>%
  bind_rows(data_frame(word = all_names_v, book_name = "All"))
  )

tidy_books2 <- tidy_books %>%
  anti_join(char_names)

# Count the most common words
tidy_books2 %>%
  count(word, sort = TRUE)

# Plot the most common words
tidy_books2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Word frequencies (cont.) -------------------------------------------------
##Comparing Austen to other authors of her time

## Download 4 of H.G. Wells' works by id
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# Tokenize
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Count the most common words
tidy_hgwells %>%
  count(word, sort = TRUE)

## Download 5 works by the Bronte sisters by id
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

# Tokenize
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Count the most common words
tidy_bronte %>%
  count(word, sort = TRUE)

## Compare the works to Austen

# Bind the works together into a single df for direct comparison
frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>% # Ensures italic words are treated the same as their flat counterparts
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)

# Plot author word frequency against Austen
ggplot(frequency, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) + # Words close to this line have similar frequencies in the compared works
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) + # Plotting as a log percent gives the data shape
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# Quantify word frequency similarities with a correlation test
cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austen`)

#
# Chapter 2: Sentiment Analysis with Tidy Data ###################################
#

#  Exporing sentiment in Austen's works ---------------------------------------

# Exploring the sentiments tibble
sentiments
View(sentiments %>% filter(lexicon == "bing"))
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# Create a tidy version of Austen's works
(tidy_books <- austen_books() %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text))

# Create a filter for "joy" words using NRC
(nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy"))

# Apply the filter to get counts of "joy" words in Emma
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

# Examine sentiment changes across Austen's novels using Bing ---------------

# Build the df to plot
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% # Join with Bing lexicon (on "word")
  count(book, index = linenumber %/% 80, sentiment) %>% # Chunk the book into 80-line segments (using integer division) and count the negative and positive sentiments in each chunk
  spread(sentiment, n, fill = 0) %>% # Put negative and positive counts for each chunk into separate columns
  mutate(sentiment = positive - negative) # Calculate net sentiment per chunk

# Plot the sentiment across each book
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


# Compare sentiment lexicons ----------------------------------------------

# Create a df holding only Pride and Prejudice
(pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice"))

# Create a df with the AFINN scores (numeric) for each chunk 
# (See line 240 for step-by-step comments)
(afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN"))

# Create a df with the Bing and NRC scores (characters) for each chunk 
(bing_and_nrc <- bind_rows( # Add new rows
  # First for Bing, which is simple pos/neg binary
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing_et_al."),
  # Then for NRC, which has 10 categories
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>% 
                # Pull out only the pos/neg words
                filter(sentiment %in% c("positive", 
                                        "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative))

# Bind the previous tibbles and plot them
bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Examine postive and negative words' contribution to analysis
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Visually examine postive and negative words' contribution to analysis
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to Sentiment",
         x = NULL) +
    coord_flip()

# The previous shows "miss" coded negative but is actually a title for young, umarried women.
# Implement custom stop word filter to solve.
(custom_stop_words <- bind_rows(data_frame(word = c("miss"),
                                          lexicon = c("custom")),
                               stop_words))

# Apply filter to analysis of words
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  anti_join(custom_stop_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Visually re-examine postive and negative words' contribution to analysis
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to Sentiment",
       x = NULL) +
  coord_flip()

# Next step: Re-run analysis, applying "miss" filter to the initial pride_prejudice df


# Wordclouds --------------------------------------------------------------

# Setting the color palette
# Viridis is supposed to be good for colorblindness and black/white printing
color_pal <- viridis_pal(alpha = 1, # Sets transparency
                         begin = 0, end = .9, # Sets color spread
                         direction = -1, # Reverses color direction
                         option = "D") # Chooses color palette

color_fun <- function(color = "A") {
  viridis_pal(alpha = 1, # Sets transparency
              begin = 0, end = .9, # Sets color spread
              direction = -1, # Reverses color direction
              option = color) # Chooses color palette
}

# ?viridis_pal()
# "magma" (or "A"), 
# "inferno" (or "B"), 
# "plasma" (or "C"), 
# "viridis" (or "D", the default option) 
# "cividis" (or "E")
# show_col(viridis_pal(option = "A")(10))

# Word cloud of the most common words
tidy_books %>%
  anti_join(stop_words) %>%
  anti_join(char_names) %>%
  count(word) %>%
  with(wordcloud(word, n, scale=c(4,.05), # Determines word size spread
                 max.words = 50, 
                 use.r.layout = TRUE, # Fixes clipping issue by relying on R instead of system layout rules
                 colors = color_pal(12) # I don't understand why this (n) works here, but it sets the number of colors and needs to be there for the line to work
                 ))
# ?wordcloud

# Word cloud of the most common positive and negative words
tidy_books %>%
  anti_join(stop_words) %>%
  anti_join(char_names) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = color_fun("D")(8),
                   max.words = 100,
                   use.r.layout = TRUE)


# Tokenizing by word groups ------------------------------------------------------

# Tokenize by sentence
(pandp_sentences <- data_frame(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences"))
# Note that this ends sentences after each ., meaning Mr. results in a new sentence.
# Mutating a UTF-8 text using iconv(text, to = 'latin1') before unnesting may help tokenizing

pandp_sentences$sentence[1]

# Tokenize by chapter
# Use regex to recognize chapters
(austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup())

# Examine one row; confirm it is one chapter
austen_chapters$chapter[2]

# Check that each book has the correct number of chapters
austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n()) 

## Find the most postive and negative chapters
# Get the negative sentiment words from the Bing df
bingneg <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

# Count all of each word
wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingneg) %>% # Join the negative Bing word df to filter for only neg words
  group_by(book, chapter) %>% # Group the df by book and chapter for the summarize call
  summarize(negativewords = n()) %>% # Count how many negative words
  left_join(wordcounts, by = c("book", "chapter")) %>%  # Join the wordcounts df
  mutate(ratio = negativewords/words) %>% # Determine the ratio of negative to total words
  filter(chapter != 0) %>% # Remove "chapter 0" which is header data
  top_n(1) %>% # Show only the most negative chapter from each book
  ungroup()

#
# Chapter 3: Analyzing Word and Document Frequency: tf-idf ###################################
#

# TF: Term frequency, how often a word appears in a document (often used with a list of stop words to account for common, unimportant words)
# IDF: Inverse document frequency, weights common/uncommon words
# TF-IDF: TF and IDF multiplied together, resulting in the frequency of a term adjusted for how rarely it's used (instead of eliminating words as stop words)

# Count each word in all Austen's works, grouped by book
(book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup())

# Count total of all words in all Austen's works, grouped by book
(total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n)))

# Combine the two
(book_words <- left_join(book_words, total_words))

# Plot the frequency of the words
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Examine the frequency by "rank"
freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         term_frequency = n/total)

# Plot
freq_by_rank %>%
  ggplot(aes(rank, term_frequency, color = book)) +
  geom_line(size = 1.1, alpha = .8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# Find the exponent for the power law in a subset of the data
rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)
lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
# This results in something close to Zipf's Law, which expects appx. a -1 slope

# Plot that subset's power law against the rest of the data
freq_by_rank %>%
  ggplot(aes(rank, term_frequency, color = book)) +
  # Plot a line showing subset's slope
  geom_abline(intercept = -0.6226, slope = -1.1125, # From the query at line 479
              color = "gray50", linetype = 2) +
  # Plot the real data
  geom_line(size = 1.1, alpha = .8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


# Austen’s tf-idf ---------------------------------------------------------

# View the frequency and adjusted frequency for each word
book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words
# idf will be zero for words that appear in all of the documents, and a higher number for words that occur in fewer of the documents

# Reorder the table to show high tf_idf words first (words that occur frequently, in fewer documents)
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))
# The most common names not common across the corpus are proper names

# Visualize
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~book, ncol = 2, scales = "free") +
    coord_flip()
# Shows Austen used similar language across her six novels. The distinguishing feature between them are the names of her characters and locations.


# Corpus of physics texts -------------------------------------------------

physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
                              meta_fields = "author")

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()
physics_words

# Calculate tf-idf
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

# Plot the results
plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~author, ncol = 2, scales = "free") +
    coord_flip()

# Eximine weird words
physics %>%
  filter(str_detect(text, " AC"))

# Create a custom stop words list and filter with it
mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn",
                                   "fig", "figure", "file", "cg", "cb", "cm"))
physics_words <- anti_join(physics_words, mystopwords, by = "word")

# Repeat the analysis
# Calculate tf-idf
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

# Plot the results
plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~author, ncol = 2, scales = "free") +
    coord_flip()

#
# Chapter 4: Relationships beween words: n-grams and correlations ###################################
#

# Tokenize by bigram
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Look at the most common bigrams
austen_bigrams %>%
  count(bigram, sort = TRUE)

# Split out the bigrams for filtering
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filter out the stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# New bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Reunite bigrams
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

## Working with ngrams, order of operations
# tokenize -> separate -> filter -> count -> unite (can switch the last 2)

# tokenize/sep/filter/count(/unite) trigrams
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
  #unite(trigram, word1, word2, word3, sep = " ")

# Search for common streets using bigrams
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# Analyze tf-idf of bigrams
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

# Plot the results
bigram_tf_idf %>%
  group_by(book) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Examine negating words
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

afinn <- get_sentiments("afinn")

# Consider which words were most frequently preceded by "not"
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

# Plot the reversed sentiment words by impact
not_words %>%
  mutate(contribution = n * score) %>% # Compute how much impact each reversed word had
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    xlab("Words preceded by \"not\"") +
    ylab("Sentiment score * number of occurrences") +
    coord_flip()

# Look at more than just "not"
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>% # Filter for negation words
  inner_join(afinn, by = c(word2 = "word")) %>% # Join the AFINN scores
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

negated_words %>%
  group_by(word1) %>% # within each negative word...
  top_n(20) %>% # ...get the top x
  ungroup() %>%
  mutate(contribution = n * score) %>% # Compute how much impact each reversed word had
  #arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>% # Need to figure out how to order within the groups
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    xlab("Negated words") +
    ylab("Sentiment score * number of occurrences") +
    facet_wrap(~ word1, scales = "free") +
    coord_flip()


# Bigram networks with igraph and ggraph ----------------------------------

# Original counts
bigram_counts

# Filter for common combinations
# Must use a separated, not united, set of bigrams
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

set.seed(2017)

# Basic bigram word relation graph
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Prettified
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# Functions for graphing bigrams ------------------------------------------

# Required libraries
# library(dplyr)
# library(tidyr)
# library(tidytext)
# library(stringr)
# library(ggplot2)
# library(igraph)
# library(ggraph)

count_bigrams <- function(dataset, stop_words) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
      geom_edge_link(show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
}

## Testing out the functions
kjv <- gutenberg_download(10)

# Count bigrams
kjv_bigrams <- kjv %>%
  count_bigrams(stop_words)

# Filter rare combos and digits, then visualize with a directed graph
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()


# Word pairs with widyr ---------------------------------------------------
library(widyr)

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

# Count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

# Find words that most often occur with Darcy
word_pairs %>%
  filter(item1 == "darcy")

# Correlation among words, how often they appear together relative to how often they appear separately
# The Phi Coefficient (known as the Pearson Correlation in other data structures) is a numerical calculation expressing the correlation between words.

# Filter for common words, then find the phi coefficient for pairs
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

# Find words most correlated with pounds
word_cors %>%
  filter(item1 == "pounds")

# Choose some interesting words and visualize their common associations
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()

# Visualize relations with a grouped ggraph
set.seed(2016)
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()

#
# Chapter 5: Converting to and from nontidy formats ###################################
#

#install.packages("topicmodels")
library(tm)
library("broom")


data(AssociatedPress, package = "topicmodels")
AssociatedPress

terms <- Terms(AssociatedPress)
head(terms)

# Make the data tidy
(ap_td <- tidy(AssociatedPress)) # Does not include values at 0

# Analyze the sentiments
(ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")))

# Visualize the top contributors to pos/neg sentiments
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ylab("Contribution to sentiment") +
    coord_flip()



