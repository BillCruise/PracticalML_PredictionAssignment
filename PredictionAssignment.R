# Practical Machine Learning - Prediction Assignment

library(caret)
library(rpart)
library(randomForest)
library(rattle)

# read data from file
pml.training <- read.csv("pml-training.csv", na.strings=c("", "NA", "#DIV/0!"))
pml.testing <- read.csv("pml-testing.csv")

# clean data
# remove columns that are all NA
removeCols <- apply(pml.training, 2, function(x) { sum(is.na(x)) })
clean.training <- pml.training[, which(removeCols == 0)]
clean.testing <- pml.testing[, which(removeCols == 0)]

# remove user name and timestamp columns
removeCols <- c(1, 2, 3, 4, 5, 6, 7)
clean.training <- clean.training[-removeCols]
clean.testing <- clean.testing[-removeCols]

# split the data into training and testing sets
inTrain <- createDataPartition(y=clean.training$classe, p=0.7, list=FALSE)
training <- clean.training[inTrain,]
testing <- clean.training[-inTrain,]
dim(training); dim(testing)


# fit a decision tree model to the training data
modFit <- train(classe ~ ., method="rpart", data=training)
print(modFit$finalModel)

# plot the tree
fancyRpartPlot(modFit$finalModel)

# check prediction error rate on training data set
pred.train <- predict(modFit, newdata=training)
confusionMatrix(pred.train, training$classe)

# check prediction error rate on testing data set
pred.test <- predict(modFit, newdata=testing)
confusionMatrix(pred.test, testing$classe)

