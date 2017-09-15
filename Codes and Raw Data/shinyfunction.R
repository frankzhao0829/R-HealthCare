HRintervals <- as(as.character(unique(data$HR_RATE)), "list")
ACTintervals <- as(as.character(unique(data$Activity)), "list")
GSRintervals <- as(as.character(unique(data$GSR_RESISTANCE)), "list")
RRintervals <- as(as.character(unique(data$RRI_INTERVAL)), "list")
SKNintervals <- as(as.character(unique(data$SKTMP_TEMPERATURE)), "list")

groupPick <- function(x){
  
  if(names(x)== "HR_RATE"){
   return(HRintervals) 
  }
  
  if(names(x)== "Activity"){
    return(ACTintervals) 
  }
  
  if(names(x)== "GSR_RESISTANCE"){
    return(GSRintervals) 
  }
  
  if(names(x)== "RRI_INTERVAL"){
    return(RRintervals) 
  }
  
  if(names(x)== "SKTMP_TEMPERATURE"){
    return(SKNintervals) 
  }
  
  
}

resultsfinal <<- function(zzz){
  if(nrow(zzz)>0){
    binPick <<- groupPick(zzz) 
    allcpquery <<- ldply(.data = binPick, .fun = function(x){
      eventz <<- paste("(", names(zzz), "=='", as.character(x), "')", sep = "")
      queryresults <<- cpquery(fitted, event = eval(parse(text = eventz)), evidence = eval(parse(text = eventss)) )
      return(queryresults)
    })
    
    qresults<<- t(allcpquery)
    colnames(qresults) <- binPick
    
    max_val <<- which.is.max(qresults)
    text_col <<- c(rep("black", length(qresults)))
    text_col[max_val] <<- "red"
    
    return(qresults)
  }
  
  if(nrow(zzz)==0){
    
    eventz <<- isolate(paste("(", names(updateData())[as.numeric(loopProbs)], "=='", as.character(updateData()[1,as.numeric(loopProbs)]), "')", sep = ""))
    queryresults <<- matrix(cpquery(fitted, event = eval(parse(text = eventz)), evidence = eval(parse(text = eventss)) ))
    colnames(queryresults)<<- isolate(names(updateData())[as.numeric(loopProbs)])
    
    return(queryresults)
  } 
}




##################################################################
###################################################################

#function for query sabre stuff 2


#queryHours <- function(x){

 
#  filter.row <-  test.data[30150+tt, ]
 # filter.col <- filter.row[,c(4:21)]  
#  serverEvidence<- paste("(",  names(filter.col), "=='",
#                      sapply(filter.col[1,], as.character), "')",
 #                     sep = "", collapse = " & ") 
#  query12 <- cpquery(fitted.sabre, event = (Incident=='Twelve Hours'), evidence = eval(parse(text = serverEvidence)) )
 # query6 <- cpquery(fitted.sabre, event = (Incident=='Six Hours'), evidence = eval(parse(text = serverEvidence)) )
  #queryClear <- cpquery(fitted.sabre, event = (Incident=='Clear'), evidence = eval(parse(text = serverEvidence)) )
  
#  queryRes <- c(query12*100, query6*100)
  
 # return(queryRes)

#}


queryAll <- function(x){ 
  #x<- uni.server[4]
  
  filterData <- filter(fixData, Server == x)
  filterData <- filterData[30100+tt,]
  epochData <- filterData$Epoch
  filterData <- filterData[,22]
  epochTimes <- c((epochData - 43200):epochData)
  runDataX <- filter(runData, sequenceID %in% x & eventID %in% epochTimes)
  
  
  incTimes <- epochData:(epochData + 43200)
  nextIncident <- filter(runData, sequenceID %in% x & eventID %in% incTimes & items == "INCIDENT")
  
  if(nrow(nextIncident) >= 1){
    incidenthours <- nextIncident$eventID - epochData
    which.inc <- nextIncident[which.min(incidenthours), 2]
    hr_inc <- paste(round((which.inc-epochData)/3600, 2), "HOURS", sep= " ")
    hr_other <- 1
  } 
  
  if(nrow(nextIncident) < 1){
    hr_inc <- "NONE"
    hr_other <- 3
  }
  
  #x<- "emghlp176"
  #epochData <- 1429173600
  #filterData <- "Trial"
  
  if(filterData != "INCIDENT" & filterData != "NR"){ 
    
    
    if(nrow(runDataX) >= 1){
      
      for(kk in 1:nrow(runDataX)){
        runDataX[kk,4] <- c(gsub("\\t", ",", runDataX[kk,4]))
      }
      
      novelEvents <- runDataX[nrow(runDataX),4]
      
      #add incident into future to check for support because incident is part of rules and must be accounted for
      runDataX <- rbindlist(list(runDataX, data.table(runDataX[1,1], epochData+300, 1, "INCIDENT")))
      runDataX <- data.frame(runDataX)
      
      a_list <- vector(mode= "list", length=nrow(runDataX))
      for(jk in 1:nrow(runDataX)){
        a_list[jk] <- c(strsplit(paste(runDataX[jk,4]), ", "))
      }
      names(a_list) <- paste(runDataX$eventID)
      trans1 <- as(a_list, "transactions")
      names(itemsetInfo(trans1)) <- "eventID"
      itemsetInfo(trans1) <- data.frame(runDataX$sequenceID, runDataX$eventID, runDataX$SIZE)
      names(itemsetInfo(trans1)) <- c("sequenceID", "eventID", "SIZE")
      
      #check support of sequences in transactions subset 
      supp<- support(ss, trans1)
      #can use ss or rules here just make sure they are in same order
      supp.df<- data.frame(Server = x, Anomaly = "WARNING", as(rules, "data.frame"), support = supp)
      supp.filter<- filter(supp.df, support.1==1)
      names(supp.filter) <- c("Server", "Anomaly", "Rule", "Support", "Confidence", "Lift", "Message")
    }
    
    if(nrow(runDataX) < 1){
      supp.filter<- data.frame()  
      
      
    }
    
    
    if(nrow(supp.filter) < 1 & filterData != "Normal"){
      supp.filter1 <- data.frame( Server = x, Anomaly = filterData, Rule = 0, Support = 0, Confidence = 0, Lift = 0, Message = 2, Incident = as.character(hr_inc), other = hr_other, query12 = 0, queryClear = 0, novels = novelEvents)
      
    }
    
    if(nrow(supp.filter) < 1 & filterData == "Normal"){
      supp.filter1 <- data.frame( Server = x, Anomaly = filterData, Rule = 0, Support = 0, Confidence = 0, Lift = 0, Message = 3, Incident = as.character(hr_inc), other = hr_other, query12 = 0, queryClear = 0, novels = "none")
      
    }
    
    if(nrow(supp.filter) >= 1){
      filter.row <- test.data[which(test.data$Server %in% x), ]
      filter.row <-  filter.row[30100+tt, ]
      #filter.row <-  filter.row[71000+tt, ]
      # filter.row <-  filter.row[68750+tt, ]
      #filter.row <-  filter.row[71925+tt, ]
      filter.col <- filter.row[,c(4:21)]  
      serverEvidence<<- paste("(",  names(filter.col), "=='",
                              sapply(filter.col[1,], as.character), "')",
                              sep = "", collapse = " & ") 
      query12 <- suppressWarnings(cpquery(fitted.sabre, event = (Incident=='Twelve Hours'), evidence = eval(parse(text = serverEvidence))))
      #query6 <- suppressWarnings(cpquery(fitted.sabre, event = (Incident=='Six Hours'), evidence = eval(parse(text = serverEvidence)) ))
      #queryClear <- suppressWarnings(cpquery(fitted.sabre, event = (Incident=='Clear'), evidence = eval(parse(text = serverEvidence)) ))
      #queryRes <- data.frame(query12 = query12*100, queryClear = queryClear*100)
      queryRes <- data.frame(query12 = query12*100, queryClear = 0)
      
      supp.filter1 <- supp.filter[which.is.max(supp.filter$Confidence), ]
      supp.filter1$Message <- 2
      supp.filter1 <- data.frame(supp.filter1 , Incident = as.character(hr_inc), other = hr_other, queryRes, novels = novelEvents)
    }
    
  }
  
  
  
  if(filterData == "INCIDENT"){  
    supp.filter1 <- data.frame( Server = x, Anomaly = "INCIDENT", Rule = 0, Support = 0, Confidence = 0, Lift = 0, Message = 1, Incident = "INCIDENT", other = 1, query12 = 100, queryClear = 0, novels= "none")
    
  }
  
  if(filterData == "NR"){  
    supp.filter1 <- data.frame( Server = x, Anomaly = "NR", Rule = 0, Support = 0, Confidence = 0, Lift = 0, Message = 1, Incident = as.character(hr_inc), other = hr_other, query12 = 100, queryClear = 0, novels = "none")
    
  }
  
  return(supp.filter1)
  
}



#this is for shiny to use 
#window.class <- c("Twelve Hours", "Eight Hours", "Four Hours", "Clear")
#make choices for render outputs
novelColor <- c("purple", "blue", "orange", "red", "black")

boxcolor <- c("red", "yellow", "green")
boxicon <- c("exclamation-triangle", "exclamation-triangle", "thumbs-up")

options(error=NULL)
##################
#not run
#for(numbas in 1:7){
 # for(numberz in 1:4){
  #  print(paste("val2$qr",numbas, "_", numberz," <- c(val2$qr", numbas, "_",numberz, ", queryResults[[2]][[", numbas, "]][[",numberz,"]])", sep=""))
  #}}


save(GSRintervals, HRintervals, RRintervals, ACTintervals, groupPick, queryAll, resultsfinal, model.Accel, model.Alti, model.Gyro, SKNintervals, model.ensemble, data, fitted, originalData, runData, fitted.sabre, metric.names, uni.server, boxicon, boxcolor, fixData, test.data, ss, rules, novelColor, file="C:/Users/teterq/Documents/GitHub/R-RTI-ServerFailureShiny/shinyQTpro.RData", precheck = TRUE)

