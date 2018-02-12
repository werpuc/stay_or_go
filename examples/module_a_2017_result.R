library(ggplot2)
library(reshape2)

setwd("C:\\Users\\Weronika\\stay_or_go\\examples")
data = read.csv("module_a_2017_data.csv", stringsAsFactors=FALSE, header = FALSE, sep = ' ')

colnames(data) <- c("date", "good", "bad")
# data$date <- months(as.Date(data$date))
data <- melt(data, variable.name = "kind")

ggplot(data, aes(x = date, y = value, col = kind)) +
  geom_point() +
  labs(title = "Tweets ratio (2017)", x = "Date", y = "Ratio") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("module_a_2017_result_image.png")
