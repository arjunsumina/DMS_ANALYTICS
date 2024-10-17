# descriptive statistics - boston data set
library(MASS)
data <- Boston
summary(data)
write.csv(summary(data),'results_descriptive_statistics.csv')
