# Data
data <- read.csv("~/Desktop/binary.csv", header = TRUE)
str(data)
data$admit <- as.factor(data$admit)
summary(data)
barplot(prop.table(table(data$admit)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Data for Developing Predictive Model
table(train$admit)
prop.table(table(train$admit))
summary(train)

library(ROSE) 
over <- ovun.sample(admit~., data = train, method = "over", N = 376)$data
table(over$admit)
summary(over)

under <- ovun.sample(admit~., data=train, method = "under", N = 194)$data
table(under$admit)

both <- ovun.sample(admit~., data=train, method = "both",
                    p = 0.5,
                    seed = 222,
                    N = 285)$data
table(both$admit)

rose <- ROSE(admit~., data = train, N = 500, seed=111)$data
table(rose$admit)
summary(rose)

# Predictive Model (Random Forest)
library(randomForest)
rftrain <- randomForest(admit~., data = train)
rfover <- randomForest(admit~., data = over)
rfunder <- randomForest(admit~., data=under)
rfboth <-randomForest(admit~., data=both)
rfrose <- randomForest(admit~., data=rose)

# Predictive Model Evaluation with test data
library(caret)
library(e1071)
confusionMatrix(predict(rftrain, test), test$admit, positive = '1')
confusionMatrix(predict(rfover, test), test$admit, positive = '1')
confusionMatrix(predict(rfunder, test), test$admit, positive = '1')
confusionMatrix(predict(rfboth, test), test$admit, positive = '1')
confusionMatrix(predict(rfrose, test), test$admit, positive = '1')
