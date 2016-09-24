#Random Forest

require(ggplot2)
require(lattice)
require(caret)
require(rattle)
require(rpart)
require(rpart.plot)
require(RColorBrewer)
require(stats)
require(corrplot)
require(randomForest)
set.seed(12345)

train <- read.csv("G:/pml-training.csv")
test <- read.csv("G:/pml-testing.csv")

#filter <- grep("belt|arm|dumbell",names(train))
#train <- train[,filter]
#test <- test[,filter]

inTrain <- createDataPartition(y = train$classe,p=0.75,list=FALSE)
myTrain <- train[inTrain,]
myTest <- train[-inTrain,]

myDataNZV <- nearZeroVar(myTrain, saveMetrics=TRUE)
myNZVvars <- names(myTrain) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
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
myTrain <- myTrain[!myNZVvars]

myTrain <- myTrain[c(-1)]

tt <- myTrain #creating another subset to iterate in loop
for(i in 1:length(myTrain)) { #for every column in the training dataset
  if( sum( is.na( myTrain[, i] ) ) /nrow(myTrain) >= .6 ) { #if n?? NAs > 60% of total observations
    for(j in 1:length(tt)) {
      if( length( grep(names(myTrain[i]), names(tt)[j]) ) ==1)  { #if the columns are the same:
        tt <- tt[ , -j] #Remove that column
      }   
    } 
  }
}

myTrain <- tt
rm(tt)

clean1 <- colnames(myTrain)
clean2 <- colnames(myTrain[,-58]) #classe removed
myTest <- myTest[clean1]
test <- test[clean2]

for (i in 1:length(test) ) {
  for(j in 1:length(myTrain)) {
    if( length( grep(names(myTrain[i]), names(test)[j]) ) ==1)  {
      class(test[j]) <- class(myTrain[i])
    }      
  }      
}
#And to make sure Coertion really worked, simple smart ass technique:
test <- rbind(myTrain[2, -58] , test) #note row 2 does not mean anything, this will be removed right.. now:
test <- test[-1,]



modFitRand <- randomForest(classe ~. , data=myTrain)
predictionRand <- predict(modFitRand, myTest, type = "class")
confusionMatrix(predictionRand, myTest$classe)