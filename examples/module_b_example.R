library(ggplot2)

data <- read.csv2("API_POL_DS2_en_csv_v2.csv", sep = ',', check.names = FALSE, na.strings = '', stringsAsFactors=FALSE)
data <- data[, 2:62]
data$na_count <- apply(data, 1, function(x) sum(is.na(x)))
data[ data$na_count == 1, c(2, 62)]

#

infant_mortality <- melt(data[666, 4:61], measure.vars = colnames(data[666, 4:61]), variable.name = 'year', value.name = 'value', na.rm = TRUE)
infant_mortality$value <- as.numeric(infant_mortality$value)
infant_mortality$year <- as.numeric(as.character(infant_mortality$year))

n <- nrow(infant_mortality)

plot <- ggplot(infant_mortality, aes(x = year, y = value)) +
  geom_point() +
  stat_smooth(formula = y ~ x, n = n) +
  ylab("Mortality rate") +
  xlab("Year") + 
  ggtitle("Mortality rate, infant (per 1,000 live births)") 

# Is there any dependency?  
cor <- cor(infant_mortality$year, infant_mortality$value)
# -0.94

model <- loess(value ~ year, data = infant_mortality, surface = 'direct')
pred_2020 <- predict(model, 2020, se = TRUE)[1]
pred_2020 <- as.numeric(pred_2020)

if ( pred_2020 < infant_mortality[n, 'value']){
  result <- "Point for Poland"
} else {
  result <- "No points awarded"
}

plot + geom_point(aes(x = 2020, y = pred_2020), color = 'red')
ggsave("module_b_result_image.png")

print(result)

