##############################################################################################################
##############################################################################################################
##
## HAR CLASSIFIER R 
## UTILIZED STACKED MACHINE LEARNING TECHNIQUES -- RANDOM FOREST
##
##
##############################################################################################################

require(zoo)
library(plyr)
library(dplyr)
library(data.table)
library(caret)
library(caretEnsemble)
#library(kernlab)
require(randomForest)
require(doParallel)
#library(nnet) 

###########################
### NOTE ###
## A lot of this stuff can be made into a function or loop and run through the "apply" family or with Plyr/dplyr packages for faster/cleaner output
## Need to go back through and set code to run in fuctions or loops


# Read in the data
data <-read.csv(file="C:/Users/teterq/Desktop/HAR/bandData.csv", stringsAsFactors = FALSE)
data <- data[ ,c(1,10:12,17:19,32:38)]


#clean up
data <- data[data$CTIVITY_TRAIN %in% c("SITTING", "WALKING", "WALKING_UP", "WALKING_DOWN"), ]
colnames(data) <- c("Activity", c(colnames(data))[2:length(data)]) #rename columns
data <- data[!(rowSums(data[,2:length(data)])==0),] #remove columns with entire rows of zero

#split data into training and testing data
inTrain <- createDataPartition(y= data$Activity, p=.75, list=FALSE )
training <- data[inTrain,]
testing <- data[-inTrain, ]

#split into separate classifier groups
Accel <- training[ , 1:4]
Gyro <- training[ , c(1, 5:7)]
Alti <- training[ , c(1, 8:14)]

#remove all zeros from individual sets
Accel <- filter(Accel, ACCEL_X != 0)
Gyro <- filter(Gyro, GYRO_X != 0)
Alti <- filter(Alti, ALT_RATE != 0)

#put data frames into a list
#data.list <- list(Accel, Gyro, Alti)

#clean up testing model
testing[testing==0]<- NA
testing <- data.frame(testing[,1], na.locf(testing[,2:length(testing)])) #na.locf function finds last observed data point
testing <- testing[complete.cases(testing), ] #removes some rows at top that had no previous data to fill in NAs with
colnames(testing) <- colnames(data)



##################### train models with training data in parallel --- "rf" stands for randomForest #############
#set.seed(44444)
#run models in parallel

n <- 50 #number of trees to use

#fine tune cross validation parameters
fitControl <- trainControl(method = "cv", number = round(sqrt(length(Accel)-1)), returnResamp = "all")

#create model with parallel registered
cl <- makeCluster(detectCores()-1) #set max cores minus 1 to leave a core open for other programs
registerDoParallel(cl)
start.time <- Sys.time()
model.Accel <- train(Activity~., method = "rf", data= Accel, trControl = fitControl, ntree = n)
stop.time <- Sys.time()
Exe.time <- stop.time-start.time
Exe.time
stopCluster(cl)

#fine tune cross validation parameters
fitControl <- trainControl(method = "cv", number = round(sqrt(length(Gyro)-1)), returnResamp = "all") 

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
start.time <- Sys.time()
model.Gyro <- train(Activity~., method = "rf", data= Gyro, trControl = fitControl, ntree = n)
stop.time <- Sys.time()
Exe.time <- stop.time-start.time
Exe.time
stopCluster(cl)

#fine tune cross validation parameters
fitControl <- trainControl(method = "cv", number = round(sqrt(length(Alti)-1)), returnResamp = "all")

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
start.time <- Sys.time()
model.Alti <- train(Activity~., method = "rf", data= Alti, trControl = fitControl, ntree = n)
stop.time <- Sys.time()
Exe.time <- stop.time-start.time
Exe.time
stopCluster(cl)

#########################################################################################
##################################################################################

#predict using test data
results.Accel <- predict(model.Accel, newdata= testing)
confusionMatrix(results.Accel, testing$Activity)
print(model.Accel$finalModel)


results.Gyro <- predict(model.Gyro, newdata= testing)
confusionMatrix(results.Gyro, testing$Activity)
print(model.Gyro$finalModel)


results.Alti <- predict(model.Alti, newdata= testing)
confusionMatrix(results.Alti, testing$Activity)
print(model.Alti$finalModel)


####################################################################
##############################################
#Create Ensemble and train new model --- stacked model training
#combining each votes from each - first column is actual activity
ensemble <- data.frame(testing$Activity, results.Accel, results.Gyro, results.Alti)


#seperate ensemble votes to training and test
inTrain <- createDataPartition(y= ensemble$testing.Activity, p=.75, list=FALSE )
ensemble.train <- ensemble[inTrain,]
ensemble.test <- ensemble[-inTrain, ]

#insert new factor into levels of data to replace values later
levels(ensemble.train$testing.Activity)<- c(levels(ensemble.train$testing.Activity), "NONE")
levels(ensemble.train$results.Accel)<- c(levels(ensemble.train$results.Accel), "NONE")
levels(ensemble.train$results.Gyro)<- c(levels(ensemble.train$results.Gyro), "NONE")
levels(ensemble.train$results.Alti)<- c(levels(ensemble.train$results.Alti), "NONE")

levels(ensemble.test$testing.Activity)<- c(levels(ensemble.test$testing.Activity), "NONE")
levels(ensemble.test$results.Accel)<- c(levels(ensemble.test$results.Accel), "NONE")
levels(ensemble.test$results.Gyro)<- c(levels(ensemble.test$results.Gyro), "NONE")
levels(ensemble.test$results.Alti)<- c(levels(ensemble.test$results.Alti), "NONE")


#Select random rows for individual columns
sampleReplace1 <- sample(1:nrow(ensemble.train), 10, replace=FALSE) 
sampleReplace2 <- sample(1:nrow(ensemble.train), 10, replace=FALSE)
sampleReplace3 <- sample(1:nrow(ensemble.train), 10, replace=FALSE)

#set random rows to NONE to simulate NA inputs- only insert a small percentage of "none" into the data to keep impact low
ensemble.train[1,1]<- "NONE"
ensemble.train[sampleReplace1,2] <- "NONE" 
ensemble.train[sampleReplace2,3] <- "NONE"
ensemble.train[sampleReplace3,4] <- "NONE"

#do on test data -- can add as many "NONE"s as possible into test data
sampleReplace1 <- sample(1:nrow(ensemble.test), 10, replace=FALSE) 
sampleReplace2 <- sample(1:nrow(ensemble.test), 10, replace=FALSE)
sampleReplace3 <- sample(1:nrow(ensemble.test), 10, replace=FALSE)

ensemble.test[1,2:4]<- "NONE"
ensemble.test[sampleReplace1,2] <- "NONE"
ensemble.test[sampleReplace2,3] <- "NONE"
ensemble.test[sampleReplace3,4] <- "NONE"
ensemble.testActivity <- ensemble.test[,1]
ensemble.test <- ensemble.test[,2:4]


################################################################################################################
#run RF on ensemble data to predict activity based on individual votes from previous models
#fine tune cross validation parameters
fitControl <- trainControl(method = "cv", number = round(sqrt(length(ensemble)-1)), returnResamp = "all")

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
start.time <- Sys.time()
model.ensemble<- train(testing.Activity~., method = "rf", data= ensemble.train, trControl = fitControl, ntree = n)
stop.time <- Sys.time()
Exe.time <- stop.time-start.time
Exe.time
stopCluster(cl)

results.ensemble <- predict(model.ensemble, newdata = ensemble.test)
confusionMatrix(results.ensemble, ensemble.testActivity)
print(model.ensemble$finalModel)


###################################################################################################################
#final.ensemble shows up the results in data frame form
final.ensemble <- data.frame(ensemble.testActivity, ensemble.test, results.ensemble)
#write out results if needed
write.csv(final.ensemble, "C:/Users/teterq/Desktop/HAR/ensembleOutput.csv" , row.names =FALSE)


##############
##  #QTpro  ##
##   #HPE   ##
##############