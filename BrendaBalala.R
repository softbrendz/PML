#Brenda M. Balala
#Week 4 Assignment-Practical Machine Learning(Prediction)
#################
setwd("d:/Module3")

#Initialization
install.packages("caret")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("e1071")
install.packages("knitr")
install.packages("rattle")
install.packages("corrplot")
require(e1071)
library(e1071)
require(lattice)
require(ggplot2)
require(caret)
require(rpart)
require(rpart.plot)
require(randomForest)
require(rattle)
require(corrplot)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(knitr)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(corrplot)
set.seed(12345)

#Loading the dataset
training_data <- read.csv('pml_training.csv', na.strings = c("NA", "#DIV/0!", ""))
test_data <- read.csv('pml_testing.csv', na.strings = c("NA", "#DIV/0!", ""))

clnColumnIndex <- colSums(is.na(training_data))/nrow(training_data) < 0.95
clean_training_data <- training_data[,clnColumnIndex]

colSums(is.na(clean_training_data))/nrow(clean_training_data)

clean_training_data <- clean_training_data[,-c(1:7)]
clean_test_data <- test_data[,-c(1:7)]
dim(training_data); dim(test_data)


plot(training_data$classe, main="Frequency Levels of the variable across the observations", xlab="Levels", ylab="Frequency")

#We then partition the training data into training set and cross validation set

inTrainIndex <- createDataPartition(clean_training_data$classe, p=0.75)[[1]]
training_training_data <- clean_training_data[inTrainIndex,]

training_crossval_data <- clean_training_data[-inTrainIndex,]

inTrain<-createDataPartition(y=training_training_data$classe, p=0.7, list=FALSE)
subTraining<-training_training_data[inTrain,]
subTesting<-training_training_data[-inTrain,]
dim(subTraining); dim(subTesting)

rfMod <- train(classe ~., method='rf', subTraining, ntree=128)
rfPrediction <- predict(rfMod,subTesting)
confusionMatrix(subTesting$classe, rfPrediction)

# let's set the seed first
set.seed(2007)
modFit2<-randomForest(classe~.,subTraining,method="class")
# let's use it for prediction on subTesting
predict2<-predict(modFit2, subTesting, type="class")
#show the results
confusionMatrix(predict2, subTesting$classe)
modFit1<-rpart(classe~., data=subTraining, method="class")
rpart.plot(modFit1, main="Classification Tree", extra=100, under=TRUE, faclen=0)

predict1<-predict(modFit1, subTraining, type="class")
confusionMatrix(predict1, subTraining$classe)
# RandomForest
set.seed(12345)


set.seed(12345)
modFitRF <- randomForest(classe~ ., data = inTrain, ntree = 1000)

prediction <- predict(modFitRF, inTraining, type = "class")
confusionMatrix(prediction, testing$classe)

modFit2<-randomForest(classe~.,training_data,method="class")
# let's use it for prediction on subTesting
predict2<-predict(modFit2, training_data, type="class")
#show the results
confusionMatrix(predict2, training_data$classe)


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files("d:/Module3/BrendaBalala.R")
