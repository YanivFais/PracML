library(randomForest)
library(kernlab)
library(corrplot)
library(caret)

data_training <- read.csv("./data/pml-training.csv", na.strings= c("NA",""," "))

# clean data, remove columns with name,timestamp
data_training_NAs <- apply(data_training, 2, function(x) {sum(is.na(x))})
dataClean <- data_training[,which(data_training_NAs == 0)]
dataClean <- dataClean[8:length(dataClean)]

# create training and cross validation
inTrain <- createDataPartition(y = dataClean$classe, p = 0.7, list = FALSE)
training <- dataClean[inTrain, ]
crossval <- dataClean[-inTrain, ]

# plot a correlation matrix
corrMat <- cor(training[, -length(training)])
corrplot(corrMat, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))
# model fit
model <- randomForest(classe ~ ., data = training)

# crossvalidate
predictCrossVal <- predict(model, crossval)
confusionMatrix(crossval$classe, predictCrossVal)

# use testing data
data_test <- read.csv("./data/pml-testing.csv", na.strings= c("NA",""," "))
data_test_NAs <- apply(data_test, 2, function(x) {sum(is.na(x))})
testClean <- data_test[,which(data_test_NAs == 0)]
testClean <- testClean[8:length(testClean)]

# predict the classes of the test set
predictTest <- predict(model, testClean)
