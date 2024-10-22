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


#correlation
#Pearson correlation is often used for quantitative continuous variables that have a linear relationship
#Spearman correlation (which is actually similar to Pearson but based on the ranked values for 
#each variable rather than on the raw data) is often used to evaluate relationships involving at 
#least one qualitative ordinal variable or two quantitative variables if the link is partially linear
#Kendall’s tau-b which is computed from the number of concordant and discordant pairs is often used for
#qualitative ordinal variables
cor(data$crim,data$medv,method = "spearman")
cor(data$crim,data$medv,method = "pearson")
cor(data$crim,data$medv,method = "kendall")

# correlation for all variables
# correlation for all variables
round(cor(data),digits = 2)




#scatterplot
# scatterplot
library(ggplot2)
ggplot(data) +
  aes(x = crim, y = medv) +
  geom_point(colour = '#0c4c8a') +
  theme_minimal()+
  ggtitle('Median House Price and Crime Rate')+
  xlab('Crime Rate')+
  ylab('Median House Price')


#scatter plot for various combinations
pairs(data[, c("lstat", "rm", "medv")])

# improved correlation matrix
library(corrplot)
corrplot(cor(data[, c("lstat", "rm", "medv")]),
         method = "number",
         type = "upper" # show only upper side
)

#correlogram
# do not edit
corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag
  )
}

# edit from here
corrplot2(
  data = data[, c("indus","lstat", "rm", "medv")],
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)

# significance of correlation
# Pearson correlation test
test <- cor.test(data$medv, data$rm)
test


# linear regression for different number of independent variables
model <- lm(medv ~ crim + zn + indus + chas + nox + rm+ age + dis+ rad+ tax+ptratio+black +lstat,data = data)
summary(model)
model <- lm(medv ~ crim + zn + indus + chas + nox + rm+ age + dis+ rad+ tax+ptratio+black,data = data)
summary(model)
model <- lm(medv ~ crim + zn + indus + chas + nox + rm+ age + dis+ rad+ tax+ptratio,data = data)
summary(model)
model <- lm(medv ~ crim + zn + indus + chas + nox + rm+ age + dis+ rad+ tax,data = data)
summary(model)
model <- lm(medv ~ crim + zn + indus + chas + nox + rm+ age + dis+ rad,data = data)
summary(model)
# residuals for the model
res <- resid(model) # get list of residuals
plot(fitted(model), res, main = "Fitted Model vs Residuals")
abline(0,0)
# qqplot for residuals
qqnorm(res)
#Create density plot of residuals
plot(density(res))













