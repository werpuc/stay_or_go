library(tidyverse)
library(tidytext)

#1 - get data from file
setwd("C:\\Users\\Weronika\\stay_or_go\\examples")
original_data = read.csv("module_a_data.csv", stringsAsFactors=FALSE)
date <- substring(original_data[1, 2], 0, 10)
data <- original_data[, c('X', 'tweet')]

#2 - tokenize tweets and drop too short ones
tweets_words <- data %>%
  mutate(tweet = str_to_lower(tweet)) %>%
  unnest_tokens(word, tweet, token = "words") %>%
  filter(nchar(word) >= 3)

#3 - prepare dictionary to get rid of flection
stem_dictionary <- read_csv2("polimorfologik-2.1.txt", col_names = c("stem", "word"))

stem_dictionary <- stem_dictionary %>%
  mutate(stem = str_to_lower(stem),
         word = str_to_lower(word)) %>%
  distinct()

#4 - join data and dictionary to have plain words
tweets_words_stem <- tweets_words %>%
  select(X, word) %>%
  filter(word != 'http') %>%
  left_join(stem_dictionary, by = c("word" = "word")) %>%
  rename(word_stem = stem) %>%
  filter(!is.na(word_stem))

#5 - get sentiment dictionary
pl_word_sentiment <- read_csv("nawl-analysis.csv")
pl_word_sentiment <- pl_word_sentiment[, c("word", "category")]

#6 - join data and dictionary to categorize words
tweets_words_sentiment <- inner_join(tweets_words_stem, 
                                     pl_word_sentiment, 
                                     by = c("word_stem" = "word"))

# happines      1
# anger        -1
# sadness      -1
# fear         -1
# disgust      -1
# neutral       0
# unclassified  0

#7 - transform category into points
tweets_words_sentiment <- tweets_words_sentiment %>%
  mutate(category = case_when(.$category == 'A' ~ '-1',
                              .$category == 'H' ~ '1',
                              .$category == 'S' ~ '-1', 
                              .$category == 'F' ~ '-1',
                              .$category == 'D' ~ '-1',
                              .$category == 'N' ~ '0',
                              .$category == 'U' ~ '0'))

tweets_words_sentiment$category = as.numeric(tweets_words_sentiment$category)

#8 - get back to tweets
tweets_results <- aggregate(category ~ X, tweets_words_sentiment, sum)

#9 - summarize results for whole data
tweets_number <- nrow(tweets_results)
tweets_bad <- round(nrow(subset(tweets_results, category < 0))/tweets_number, 4)*100
tweets_good <- round(nrow(subset(tweets_results, category > 0))/tweets_number, 4)*100

cat('Results for', date, '\n', tweets_good, '% good \n', tweets_bad, '% bad')
 
# Results for 2017-01-01 
# 11.34 % good 
# 16.49 % bad