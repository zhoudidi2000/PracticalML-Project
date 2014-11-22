# Download the data and load the packages `caret` and `randomForest` into R.
setwd("d:/Program Files/RStudio/PracticalMachineLearning")
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml_training.csv")
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml_testing.csv")
library(caret)
library(randomForest)

# Data Cleaning
# Fill blank with NAs and remove all the NAs
training <- read.csv("pml_training.csv", na.string = c("NA", ""))
testing <- read.csv("pml_testing.csv", na.string = c("NA", ""))
NAs <- apply(training,2,function(x) sum(is.na(x)))
CleanTrain <- training[,which(NAs == 0)]
colnames(CleanTrain)
# Remove unrelated vaviables
CleanTrain <- CleanTrain[, -c(1:7)]

# Model Building-cross validation sets
inTrain <- createDataPartition(y = CleanTrain$classe, p=0.75, list=FALSE)
Train <- CleanTrain[inTrain, ]
Test <- CleanTrain[-inTrain, ]
# random forests algorithm
fitControl <- trainControl(method = "cv", number = 5)
set.seed(12345)
modelFit <- train(classe ~ ., data = Train,
                  method = "rf",
                  trControl = fitControl)
modelFit
# Predict Test set to get the out of sample accuracy and error rate
TestPredict<-predict(modelFit, Test)
cm<-confusionMatrix(Test$classe, TestPredict)
cm
1-sum(diag(cm$table))/sum(cm$table)


# Predict the testing set and get the answers
answers <- predict(modelFit, testing)
answers



