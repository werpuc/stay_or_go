library(tidyverse)

#1
setwd("C:\\Users\\Weronika\\stay_or_go\\examples")
original_data = read.csv("module_a_data.csv", stringsAsFactors=FALSE)
data <- original_data[, c('X', 'tweet')]

tweets_words <- data %>%
  unnest_tokens(word, tweet, token = "words") %>%
  filter(!(word %in% pl_stop_words)) %>%
  filter(nchar(word) >= 3)

head(tweets_words)

#2
pl_stop_words <- read_lines("polish_stopwords.txt")
stem_dictionary <- read_csv2("polimorfologik-2.1.txt", col_names = c("stem", "word"))

stem_dictionary <- stem_dictionary %>%
  mutate(stem = str_to_lower(stem),
         word = str_to_lower(word)) %>%
  distinct()

#3
tweets_words_stem <- tweets_words %>%
  select(X, word) %>%
  filter(word != 'http') %>%
  left_join(stem_dictionary, by = c("word" = "word")) %>%
  rename(word_stem = stem) %>%
  filter(!is.na(word_stem))

#4
tweets_words_stem %>%
  count(word_stem) %>%
  ungroup() %>%
  arrange(n) %>%
  mutate(word = factor(word_stem, levels=word_stem)) %>%
  top_n(30, n) %>%
  ggplot() +
  geom_bar(aes(word, n), stat = "identity", fill = "lightgreen", color = "gray50") +
  coord_flip()

#5
pl_word_sentiment <- read_csv("nawl-analysis.csv")
pl_word_sentiment <- pl_word_sentiment[, c("word", "category")]
tweets_words_sentiment <- inner_join(tweets_words_stem, 
                                     pl_word_sentiment, 
                                     by = c("word_stem" = "word"))

# happines     1
# anger       -1
# sadness     -1
# fear        -1
# disgust     -1
# neutral      0
# unclassified 0


tweets_words_sentiment <- tweets_words_sentiment %>%
  mutate(category = case_when(.$category == 'A' ~ '-1',
                              .$category == 'H' ~ '1',
                              .$category == 'S' ~ '-1', 
                              .$category == 'F' ~ '-1',
                              .$category == 'D' ~ '-1',
                              .$category == 'N' ~ '0',
                              .$category == 'U' ~ '0'))

tweets_words_sentiment$category = as.numeric(tweets_words_sentiment$category)

tweets_results <- aggregate(category ~ X, tweets_words_sentiment, sum)

result <- sum(tweets_results$category)
