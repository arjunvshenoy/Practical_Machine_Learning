library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

set.seed(10000)

setwd("C:/Users/212312659/Desktop/Utility/Essentials/Coursera/Practical Machine Learning/Project")
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

## PARTITIONING THE DATA
temp <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
training_train <- training[temp, ]; 
training_test <- training[-temp, ]

## CLEANING OF DATA
training_train_NZV <- nearZeroVar(training_train, saveMetrics=TRUE)
variables <- names(training_train) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                      "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                      "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                      "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                      "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                      "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                      "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                      "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                      "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                      "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                      "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                      "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                      "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                      "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                      "stddev_yaw_forearm", "var_yaw_forearm")
training_train <- training_train[!variables]

training_train <- training_train[c(-1)]
training_train_1 <-training_train

training_train_new <- training_train 
for(i in 1:length(training_train)) { 
  if( sum( is.na( training_train[, i] ) ) /nrow(training_train) >= .5 ) { 
    for(j in 1:length(training_train_new)) {
      if( length( grep(names(training_train[i]), names(training_train_new)[j]) ) ==1)  { 
        training_train_new <- training_train_new[ , -j] 
      }   
    } 
  }
}

training_train <- training_train_new

training_test <- training_test[colnames(training_train)]

## FITTING A DECISION TREE
model_DT <- rpart(classe ~ ., data=training_train, method="class")

pred <- predict(model_DT, training_test, type = "class")

confusionMatrix(pred, training_test$classe)

## FITTING A RANDOM FOREST 
model_RF <- randomForest(classe ~ ., data=training_train, importance = FALSE)

pred <- predict(model_RF, training_test, type = "class")

confusionMatrix(pred, training_test$classe)


## PREDICTIONS
testing <- testing[colnames(training_train[, -58])]
pred <- predict(model_RF, testing, type = "class")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(pred)
