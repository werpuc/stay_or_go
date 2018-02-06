library(reshape2)
library(ggplot2)

setwd("C:\\Users\\Weronika\\stay_or_go\\examples")
original_data = read.csv("WYCH_2958_CTAB_20180206200238.csv", sep = ';', dec = ' ', stringsAsFactors=FALSE)

data <- original_data[3:16]
colnames(data) <- substr(colnames(data), 86, 89)

data <- melt(data, measure.vars = colnames(data), variable.name = 'year', value.name = 'value', na.rm = TRUE)

data$value <- as.numeric(gsub(",", ".", data$value))
data$year <- as.numeric(as.character(data$year))

ggplot(data, aes(x = year, y = value)) + 
  geom_point() + 
  geom_smooth()

