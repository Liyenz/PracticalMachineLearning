
## Get the working directory and load the dataset.
getwd()
training = read.csv("pml-training.csv",header=TRUE, na.strings=c("NA","#DIV/0!",""))
head(train)
testing = read.csv("pml-testing.csv",header=TRUE, na.strings=c("NA","#DIV/0!",""))
head(test)


## Load some libraries needed for this assignments. 
## If library has not installed, install it accordingly. 
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

set.seed(12345)

## Group the dataset into 2 sets with 60% of the original data going to the training set and 40% to the testing set. The model built with the training dataset, then tested on the testing dataset using following code
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]
myTesting <- training[-inTrain, ]
dim(myTraining)
dim(myTesting)

## Output
## [1] 11776   160
## [1] 7846  160

## Remove first column of Dataset - ID Removing first ID variable so that it does not interfere with ML Algorithms:
myTraining <- myTraining[c(-1)]

## Cleaning NAs
trainingV3 <- myTraining 
for(i in 1:length(myTraining)) { #for every column in the training dataset
  if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .6 ) { #if n?? NAs > 60% of total observations
    for(j in 1:length(trainingV3)) {
      if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
        trainingV3 <- trainingV3[ , -j] #Remove that column
      }   
    } 
  }
}

dim(trainingV3)
## Output
## [1] 11776    59

## Use the above as my dataset
myTraining <- trainingV3
rm(trainingV3)

## Do the same on the Testing Data
clean1 <- colnames(myTraining)
clean2 <- colnames(myTraining[, -59]) #already with classe column removed
myTesting <- myTesting[clean1]
testing <- testing[clean2]

dim(myTesting)
## Output
## [1] 7846   59

## In order to ensure Decision Trees and especially RandomForest Algorithm with the Test data set,  we need to coerce the data into the same type.

for (i in 1:length(testing) ) {
  for(j in 1:length(myTraining)) {
    if( length( grep(names(myTraining[i]), names(testing)[j]) ) ==1)  {
      class(testing[j]) <- class(myTraining[i])
    }      
  }      
}

testing <- rbind(myTraining[2, -59] , testing) 
testing <- testing[-1,]

## ML Algorithm for prediction: Decision Tree 
modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")
fancyRpartPlot(modFitA1)
predictionsA1 <- predict(modFitA1, myTesting, type = "class")
confusionMatrix(predictionsA1, myTesting$classe)

## ML Algorithm for prediction: Random Tree
modFitB1 <- randomForest(classe ~. , data=myTraining)
predictionsB1 <- predict(modFitB1, myTesting, type = "class")
confusionMatrix(predictionsB1, myTesting$classe)

## File for assignment submission.
predictionsB2 <- predict(modFitB1, testing, type = "class")
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)

