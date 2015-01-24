
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

# Change the 
# levels(testing$new_window) <- levels(training$new_window)


# Install randomForest package
# install.packages("randomForest")
library(randomForest)
# install.packages("caret")
library(caret)


set.seed(111)
# Define cross-validation experiment
cv.control = trainControl(method = "cv", number = 2)
# Perform the cross validation
cv <- train(classe ~ ., data = training, method = "rf", trControl = cv.control)
rf <- randomForest(classe ~ ., data = training, mtry = cv$bestTune$mtry)

predict.train <- predict(rf)
table(predict.train, training$classe)

predict.test <- predict(rf, newdata = testing)
table(predict.test, testing$classe)


predict.clean.test <- predict(rf, newdata = clean.testing)
predict.clean.test


# Function to write a vector to files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_", i ,".txt")
        write.table(x[i], file = filename, quote = FALSE,
                    row.names = FALSE, col.names = FALSE)
    }
}