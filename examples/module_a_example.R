library(tidytext)

# setwd("C:\\Users\\Weronika\\stay_or_go\\examples")
original_data = read.csv("module_a_data.csv", stringsAsFactors=FALSE)
data <- original_data[, c('X', 'tweet')]

tweets_words <- data %>%
  unnest_tokens(word, tweet, token = "words")

head(tweets_words)


