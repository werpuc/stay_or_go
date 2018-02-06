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
  geom_smooth(method = 'lm') +
  labs(y = 'Odsetek dzieci objetych wychowaniem przedszkolnym', x = 'Rok', title = 'Ogolem dzieci w wieku 3 - 5 lat')

ggsave("module_c_result_image.png")

model <- lm(value ~ year, data)

#y = a*x +b
a <- as.numeric(coef(model)[2])
b <- as.numeric(coef(model)[1])

if (a > 1){
  result <- "Point for Poland."
} else if (a < 1 & a > -1) {
  result <- "Zero points."
} else {
  result <- "Point from Poland."
}

print(result)
# Point for Poland