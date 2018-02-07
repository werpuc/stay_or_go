setwd("C:\\Users\\Weronika\\stay_or_go\\examples")
data = read.csv("module_a_2017_data.csv", stringsAsFactors=FALSE, header = FALSE, sep = ' ')

colnames(data) <- c("date", "tweets_good", "tweets_bad")
head(data)

ggplot(data, aes(x = date, y = tweets_good)) +
  geom_point()

ggplot(data, aes(x = date, y = tweets_bad)) +
  geom_point()
