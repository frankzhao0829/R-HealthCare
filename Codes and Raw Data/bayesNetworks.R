##
##############################################################################################################

library(lubridate)
library(xts)
library(reshape2)
require(zoo)
library(plyr)
library(dplyr)
library(data.table)
require(bit64)
library(caret)
library(caretEnsemble)
#library(kernlab)
require(randomForest)
require(doParallel)
#library(nnet)
library(bnlearn)
#library(Rgraphviz)
library(csvread)
#library(DMwR)


data <-read.csv(file="C:/Users/teterq/Desktop/HAR/bandData.csv", stringsAsFactors = FALSE)
#data <- data[ ,c(1,10:12,16:20,22,25,32:38)]
data <- data[ ,c(1,16,20,22,25)]

#clean up
data <- data[data$CTIVITY_TRAIN %in% c("SITTING", "WALKING", "WALKING_UP", "WALKING_DOWN"), ]
colnames(data) <- c("Activity", c(colnames(data))[2:length(data)]) #rename columns
data <- data[!(rowSums(data[,2:length(data)])==0),] #remove columns with entire rows of zero

data[data==0]<- NA
data <- data.frame(data[,1], na.locf(data[,2:length(data)])) #na.locf function finds last observed data point
data <- data[complete.cases(data), ] #removes some rows at top that had no previous data to fill in NAs with
colnames(data) <- c("Activity", c(colnames(data))[2:length(data)])

#save for other use before discreatize
originalData <- data

#make discrete
data1 <- bnlearn::discretize(data[,2:length(data)], method = 'quantile', breaks= c(4,4,4,4) )
data <- cbind(data$Activity, data1)
colnames(data) <- c("Activity", c(colnames(data))[2:length(data)])

#hill climbing
bn.hc <- hc(data)

#bn.fit model
fitted <- bn.fit(bn.hc, data)

#save to bif file
#write.bif(file="C:/Users/teterq/Desktop/HAR/bn_fit.bif", fitted)


#graphviz.plot(fitted)

#& SKTMP_TEMPERATURE == FALSE & ALT_STEPPING_GAIN_LOSS == FALSE & ACCEL_X ==FALSE & ACCEL_Y == FALSE & ACCEL_Z==FALSE & GYRO_X == FALSE & GYRO_Y == FALSE & GYRO_Z == FALSE & ALT_STEPPING_GAIN == FALSE & ALT_STEPPING_ASCENDED == FALSE & ALT_STEPPING_DESCENDED == FALSE & ALT_TOTAL_GAIN == FALSE & ALT_TOTAL_LOSS==TRUE

#query probabilities
#cpquery(fitted, event=(Activity == "SITTING"), evidence = (HR_RATE == "[64,72]") & (SKTMP_TEMPERATURE == "[29.1,29.8]" )) 

###################
#create a low heart rate
data[,4]<- as.character(data$HR_RATE)

data[118:160,4] <- 0

originalData[100:105, 4]<- 80
originalData[106:109, 4]<- 90
originalData[110:114, 4]<- 97
originalData[115:117, 4]<- 108
originalData[118:122, 4]<- 119
originalData[123:128, 4]<- 127
originalData[129:133, 4]<- 140
originalData[134:139, 4]<- 155
originalData[140:143, 4]<- 145
originalData[144:149, 4]<- 137
originalData[150:155, 4]<- 130
originalData[156:160, 4]<- 119
originalData[161:165, 4]<- 111
originalData[166:170, 4]<- 99
originalData[171:175, 4]<- 90
originalData[176:180, 4]<- 84
originalData[181:185, 4]<- 79
originalData[186:190, 4]<- 73







####################################################################
####################################################################
## BAYES NETWORK FOR SABRE ACCOUNT ##



# Set the directory of Farm CSV files
setwd("C:/Users/teterq/Desktop/Process")
#Get file names from folder
file_list<- list.files()



emg<- csvread(file_list[3], header= TRUE, coltypes=c("string", "string", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"))
col <- gsub(pattern = "\\%", replacement = "", names(emg))
colnames(emg)<- col
del <- gsub("\"", "", names(emg))
colnames(emg) <- del
no_quote1 <- gsub("\"", "", emg$Server)
emg[,1] <- no_quote1
no_quote2 <- gsub("\"", "", emg$Epoch.Time)
emg[,2] <- no_quote2
colnames(emg) <- gsub("/", ".", names(emg))
del <- gsub("5", "", names(emg))
colnames(emg) <- del
emg$Server <- tolower(emg$Server)
emg.name <- names(emg)
emg.name <- gsub("\\r", "", emg.name)
names(emg) <- emg.name
emg1 <- data.table(emg)


emg<- csvread(file_list[4], header= TRUE, coltypes=c("string", "string", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"))
col <- gsub(pattern = "\\%", replacement = "", names(emg))
colnames(emg)<- col
del <- gsub("\"", "", names(emg))
colnames(emg) <- del
no_quote1 <- gsub("\"", "", emg$Server)
emg[,1] <- no_quote1
no_quote2 <- gsub("\"", "", emg$Epoch.Time)
emg[,2] <- no_quote2
colnames(emg) <- gsub("/", ".", names(emg))
del <- gsub("5", "", names(emg))
colnames(emg) <- del
emg$Server <- tolower(emg$Server)
emg.name <- names(emg)
emg.name <- gsub("\\r", "", emg.name)
names(emg) <- emg.name
emg2 <- data.table(emg)

emg<- csvread(file_list[9], header= TRUE, coltypes=c("string", "string", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"))
col <- gsub(pattern = "\\%", replacement = "", names(emg))
colnames(emg)<- col
del <- gsub("\"", "", names(emg))
colnames(emg) <- del
no_quote1 <- gsub("\"", "", emg$Server)
emg[,1] <- no_quote1
no_quote2 <- gsub("\"", "", emg$Epoch.Time)
emg[,2] <- no_quote2
colnames(emg) <- gsub("/", ".", names(emg))
del <- gsub("5", "", names(emg))
colnames(emg) <- del
emg$Server <- tolower(emg$Server)
emg.name <- names(emg)
emg.name <- gsub("\\r", "", emg.name)
names(emg) <- emg.name
emg3 <- data.table(emg)


emg<- csvread(file_list[12], header= TRUE, coltypes=c("string", "string", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"))
col <- gsub(pattern = "\\%", replacement = "", names(emg))
colnames(emg)<- col
del <- gsub("\"", "", names(emg))
colnames(emg) <- del
no_quote1 <- gsub("\"", "", emg$Server)
emg[,1] <- no_quote1
no_quote2 <- gsub("\"", "", emg$Epoch.Time)
emg[,2] <- no_quote2
colnames(emg) <- gsub("/", ".", names(emg))
del <- gsub("5", "", names(emg))
colnames(emg) <- del
emg$Server <- tolower(emg$Server)
emg.name <- names(emg)
emg.name <- gsub("\\r", "", emg.name)
names(emg) <- emg.name
emg4 <- data.table(emg)


emg<- csvread(file_list[6], header= TRUE, coltypes=c("string", "string", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"))
col <- gsub(pattern = "\\%", replacement = "", names(emg))
colnames(emg)<- col
del <- gsub("\"", "", names(emg))
colnames(emg) <- del
no_quote1 <- gsub("\"", "", emg$Server)
emg[,1] <- no_quote1
no_quote2 <- gsub("\"", "", emg$Epoch.Time)
emg[,2] <- no_quote2
colnames(emg) <- gsub("/", ".", names(emg))
del <- gsub("5", "", names(emg))
colnames(emg) <- del
emg$Server <- tolower(emg$Server)
emg.name <- names(emg)
emg.name <- gsub("\\r", "", emg.name)
names(emg) <- emg.name
emg5 <- data.table(emg)


emg<- rbindlist(list(emg1, emg2, emg3, emg4, emg5))

emg <- data.frame(emg)

#clean data set of NA values and values that should be zero
emg[which(is.na(emg$sys) & !is.na(emg$Tcp_InSegs)), c(6,7)]<- NA
emg[which(is.na(emg$sys) & !is.na(emg$swap_out)), 21]<- NA
emg[which(!is.na(emg$sys) & is.na(emg$Tcp_InSegs)), 6]<- 0
emg[which(!is.na(emg$sys) & is.na(emg$Tcp_OutSegs)), 7]<- 0

rm(emg1, emg2, emg4, emg5)

#remove na values and first columns for bayes network
farm.data<- filter(emg, sys != "NA") 
farm.data <- farm.data[which(!is.na(farm.data$mem_buff)),]


#make discrete

this.data1 <- bnlearn::discretize(farm.data[,4:length(farm.data)], method = 'interval', breaks = 4 )
this.data <- cbind(farm.data[,1:3], this.data1)


#create training and testing set
test.data <- emg[which(emg$Server %in% unique(emg3$Server)),]
test.discrete <- this.data[which(this.data$Server %in% unique(emg3$Server)),]
for(ll in 4:21){
  
  test.data[which(!is.na(test.data$sys)), ll] <- as.character(test.discrete[,ll])
  
}

this.data <- this.data[which(!(this.data$Server %in% unique(emg3$Server))),]

### add new column for network to give probabilities for windows before

# get incident log that was pregenerated
incidents <- fread("C:/Users/teterq/Desktop/sabre/sabre_incidentLog.csv", header=FALSE) 
incidents[, V1 := tolower(as.character(incidents$V1)) ] 
#incidents <- select(incidents, 1:2)
incidents[, V2 := as.numeric(align.time(as.POSIXlt(strptime(incidents$V2, "%m/%d/%Y %H:%M")), n=300))] 
names(incidents)<- c("Server", "Epoch") 
incidents[,items:= 1] 
incidents[, variable:= "INCIDENT"] 
#ignore warning at end, data still okay

# create column
this.data <- mutate(this.data, Incident = "Clear") 
#find all incidents in incident log that are in dataset
incidents <- incidents[Server %in% unique(this.data$Server)] 
incidents <- incidents %>% arrange(Server) %>% group_by(Server) %>% arrange(Epoch) %>% ungroup() 
incidents <- data.frame(incidents) 

#add classifiers into incident column
pb <- txtProgressBar(min = 1, max = nrow(incidents), style = 3)
for(i in 1:nrow(incidents)){
  Sys.sleep(0.1) #this temporarily suspends the function
  serverRow <- incidents[i,1]
  epochRow <- incidents[i,2]
  
  twelvehours <- epochRow - 43200
  #sixhours <- epochRow - 21600
  
  
  this.data[which(this.data$Server %in% serverRow & this.data$Epoch %in% c(twelvehours:epochRow)), 22] <- "Twelve Hours"
  #this.data[which(this.data$Server %in% serverRow & this.data$Epoch %in% c(sixhours:epochRow)),22] <- "Six Hours"

  #need to update this to take overlapping into account
  setTxtProgressBar(pb, i) #updates the progress bar
} 
close(pb) 

#this.data <- filter(this.data, Incident %in% c("Twelve Hours", "Six Hours"))
#this.data1 <- this.data[, c(4:length(this.data))]
#this.data1[] <- lapply(this.data1, as.character)
#this.data1$Incident <- factor(this.data1$Incident)

#dataSmote <- SMOTE(form= Incident ~ ., data= this.data1, perc.over = 100, k=2, perc.under=0)

filter.twelve <- filter(this.data, Incident %in% "Twelve Hours")
filter.twelve <- data.table(filter.twelve)

filter.clear <- filter(this.data, Incident %in% "Clear")
filter.clear <- sample_n(filter.clear, size = nrow(filter.twelve)*4, replace = FALSE)
filter.clear <- data.table(filter.clear)
all.data <- rbindlist(list(filter.twelve, filter.clear, filter.twelve))
all.data <- data.frame(all.data)
#all.data <- this.data

all.data$Server<- as.factor(all.data$Server)
all.data$Incident <- as.factor(all.data$Incident)
this.data1 <- all.data[, c(4:length(all.data))]




#hill climbing
bn.hc.sabre <- iamb(this.data1)

#bn.fit model
fitted.sabre <- bn.fit(bn.hc.sabre, this.data1)











#create training and testing set
test.data <- emg[which(emg$Server %in% unique(emg3$Server)),]

this.data <- farm.data[which(!(farm.data$Server %in% unique(emg3$Server))),]

### add new column for network to give probabilities for windows before

# get incident log that was pregenerated
incidents <- fread("C:/Users/teterq/Desktop/sabre/sabre_incidentLog.csv", header=FALSE) 
incidents[, V1 := tolower(as.character(incidents$V1)) ] 
#incidents <- select(incidents, 1:2)
incidents[, V2 := as.numeric(align.time(as.POSIXlt(strptime(incidents$V2, "%m/%d/%Y %H:%M")), n=300))] 
names(incidents)<- c("Server", "Epoch") 
incidents[,items:= 1] 
incidents[, variable:= "INCIDENT"] 
#ignore warning at end, data still okay

# create column
this.data <- mutate(this.data, Incident = 0) 
#find all incidents in incident log that are in dataset
incidents <- incidents[Server %in% unique(this.data$Server)] 
incidents <- incidents %>% arrange(Server) %>% group_by(Server) %>% arrange(Epoch) %>% ungroup() 
incidents <- data.frame(incidents) 

#add classifiers into incident column
pb <- txtProgressBar(min = 1, max = nrow(incidents), style = 3)
for(i in 1:nrow(incidents)){
  Sys.sleep(0.1) #this temporarily suspends the function
  serverRow <- incidents[i,1]
  epochRow <- incidents[i,2]
  
  twelvehours <- epochRow - 43200
  #sixhours <- epochRow - 21600
  
  
  this.data[which(this.data$Server %in% serverRow & this.data$Epoch %in% c(twelvehours:epochRow)), 22] <- 1
  #this.data[which(this.data$Server %in% serverRow & this.data$Epoch %in% c(sixhours:epochRow)),22] <- "Six Hours"
  
  #need to update this to take overlapping into account
  setTxtProgressBar(pb, i) #updates the progress bar
} 
close(pb) 


filter.twelve <- filter(this.data, Incident == 1)
filter.twelve <- data.table(filter.twelve)

filter.clear <- filter(this.data, Incident == 0)
filter.clear <- sample_n(filter.clear, size = nrow(filter.twelve)*7, replace = FALSE)
filter.clear <- data.table(filter.clear)
all.data <- rbindlist(list(filter.twelve, filter.clear, filter.twelve))
all.data <- data.frame(all.data)

#all.data <- this.data
this.data1 <- all.data[, c(4:length(all.data))]




#hill climbing
bn.hc.sabre <- hc(this.data1)
#plot(bn.hc.sabre)

#bn.fit model
fitted.sabre <- bn.fit(bn.hc.sabre, this.data1)

filter.row <- test.data[which(test.data$Server %in% "emghlp176"), ]
filter.row <-  filter.row[30100:30300, ]
filter.col <- filter.row[,c(4:21)] 
filter.col <- filter(filter.col, sys != "NA")



sabre.predict <- predict(fitted.sabre, "Incident", filter.col) 



