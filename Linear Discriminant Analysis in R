# Data
data("iris")
str(iris)

library(psych)
pairs.panels(iris[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[iris$Species],
             pch = 21)

# Data partition
set.seed(555)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- iris[ind==1,]
testing <- iris[ind==2,]

# Linear discriminant analysis
library(MASS)
linear <- lda(Species~., training)
linear
attributes(linear)

# Histogram
p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$Species)
ldahist(data = p$x[,2], g = training$Species)

# Bi-Plot
library(devtools)
# install_github("fawda123/ggord")
library(ggord)
ggord(linear, training$Species, ylim = c(-10, 10))

# Partition plot
library(klaR)
partimat(Species~., data = training, method = "lda")
partimat(Species~., data = training, method = "qda")

# Confusion matrix and accuracy - training data
p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$Species)
tab
sum(diag(tab))/sum(tab)

# Confusion matrix and accuracy - testing data
p2 <- predict(linear, testing)$class
tab1 <- table(Predicted = p2, Actual = testing$Species)
tab1
sum(diag(tab1))/sum(tab1)
