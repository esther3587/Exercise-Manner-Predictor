---
title: "Practical Machine Learning Final Project"
author: "Shun-Wen Chang"
date: "December 16, 2015"
output: html_document
---
##Background##
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, our goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants to predict the manner in which they did the exercise.


##Load Required Packges and Set Seeds##

```{r,echo=TRUE}
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
set.seed(3314)
```

##Getting Data##

```{r, echo=TRUE}
train <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),na.strings=c("NA","#DIV/0!",""))
test <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),na.strings=c("NA","#DIV/0!",""))

```

##Divide the Training Data Set into Two Parts##

60% for myTrain and 40% for myTest

```{r, echo=TRUE}

inTrain <- createDataPartition(y=train$classe, p=0.6, list=FALSE)
myTrain <- train[inTrain, ]
myTest <- train[-inTrain, ]
dim(myTrain)
dim(myTest)
```

##Clean the Training Data##



Process #1 : Remove near-zero-variance variables
```{r, echo=TRUE}
myNZV <- nearZeroVar(myTrain, saveMetrics=TRUE)
NZVnames <- subset(myNZV, nzv==TRUE)
myNZVvars <- names(myTrain) %in% row.names(NZVnames)
myTrain <- myTrain[!myNZVvars]
dim(myTrain)
```

Process #2 : Remove the first two columns because they are user names and IDs
```{r, echo=TRUE}
myTrain <- myTrain[c(-1,-2)]
```

Process #3 : Remove variables with over 50% NAs
```{r, echo=TRUE}
train3 <- myTrain
for(i in 1:length(myTrain)) { 
        if( sum( is.na( myTrain[, i] ) ) /nrow(myTrain) >= .5 ) { 
        for(j in 1:length(train3)) {
            if( length( grep(names(myTrain[i]), names(train3)[j]) ) ==1)  { 
                train3 <- train3[ , -j] 
            }   
        } 
    }
}

myTrain <- train3
rm(train3)
dim(myTrain)

```

##Repeat the same cleaning procedure on myTest set##
```{r, echo=TRUE}
proc1 <- colnames(myTrain)
proc2 <- colnames(myTrain[,-57])

myTest <- myTest[proc1]
test <- test[proc2]

dim(test)
```

##Transofrm Data Types##

Because the data type in training data is different from that in the test data set, we have to coerce the data into the same type.

```{r, echo=TRUE}
for (i in 1:length(test) ) {
        for(j in 1:length(myTrain)) {
        if( length( grep(names(myTrain[i]), names(test)[j]) ) ==1)  {
            class(test[j]) <- class(myTrain[i])
        }      
    }      
}

test <- rbind(myTrain[2,-57], test)
test <- test[-1,]

```

##Using Decision Tress for Prediction##

```{r, echo=TRUE}
modelDT <- rpart(classe ~ ., data = myTrain, method="class")
fancyRpartPlot(modelDT)
predictDT <- predict(modelDT, myTest, type = "class")
confusionMatrix(predictDT, myTest$classe)
```

##Using Random Forest for Prediction##

```{r, echo=TRUE}
modelRF <- randomForest(classe ~ ., data = myTrain)
predictRF <- predict(modelRF, myTest, type = "class")
confusionMatrix(predictRF, myTest$classe)

```

As you shall see, Random Forests prediction has higher accuracy, 0.9994 compared to 0.891 from Decision Trees. 



##Generating Files for Submission##

We use Random Forests for prediction since it gives better results!

```{r, echo=TRUE}
predictRF_test <- predict(modelRF, test, type ="class")

write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}


write_files(predictRF_test)
```
