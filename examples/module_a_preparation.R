library(tidyverse)

without_polish <- function(word) {
  fin = ""
  for(i in 1:nchar(word)){
    x <- substr(word, i, i)
    x <- case_when(
      x == 'ą' ~ 'a',
      x == 'ć' ~ 'c',
      x == 'ę' ~ 'e',
      x == 'ł' ~ 'l',
      x == 'ń' ~ 'n',
      x == 'ó' ~ 'o',
      x == 'ś' ~ 's',
      x == 'ż' ~ 'z',
      x == 'ź' ~ 'z'
    )
    if(is.na(x)) x <- substr(word, i, i)
    fin <- paste(fin, x, sep = '')
  }
  print(fin)
}

tweets <- read.csv("module_a_data.csv", stringsAsFactors=FALSE)

tweets_plain <- tweets %>%
  mutate(tweet = str_to_lower(tweet)) %>%
  mutate(tweet = without_polish(tweet)) 

write.csv(tweets_plain, "tweets_plain.csv")
