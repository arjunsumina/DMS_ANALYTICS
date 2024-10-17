#The Boston Housing dataset includes housing prices and related factors in the Boston area. 
#The dataset was obtained from information collected by the U.S. Census Service concerning housing 
#in the area of Boston, Massachusetts. The dataset also comprises 506 observations on 14 variables.

#The variables include:
#Crim - The crim variable represents the per capita crime rate by town.
#Zn - The zn variable represents the proportion of residential land zoned for lots over 25,000 sq.ft.
#Indus - The indus variable represents the proportion of non-retail business acres per town.
#Chas - The chas variable represents whether the property is located along the Charles River or not.
#Nox - The nox variable represents the nitrogen oxides concentration (parts per 10 million) in the air.
#Rm -The rm variable represents the average number of rooms per dwelling.
#age: This variable represents the proportion of owner-occupied units built prior to 1940.
#Dis -The dis variable represents the weighted distances to five Boston employment centres.
#Rad - The rad variable represents the index of accessibility to radial highways.
#Tax - The tax variable represents the full-value property-tax rate per $10,000.
#Ptratio - The ptratio variable represents the pupil-teacher ratio by town.
#Black - The black variable represents the proportion of blacks by town.
#Lstat - The lstat variable represents the percentage of the lower status of the population.
#Medv - The medv variable represents the median value of owner-occupied homes in $1000s.

# descriptive statistics - boston data set
library(MASS)
data <- Boston
summary(data)
write.csv(summary(data),'results_descriptive_statistics.csv')
print("test")

#box plot
boxplot(data$crim)
boxplot(data$crim,outline = FALSE)
boxplot(data$crim,data$medv,outline = FALSE,names=c("Crime Rate","Price"), main = 'Crime Rate and Price Boxplots')



