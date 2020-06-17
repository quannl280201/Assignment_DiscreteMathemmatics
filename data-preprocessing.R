# Import data
rm(list=ls())
library(readxl)
library(dplyr)
file_6 <- read_excel("D:/quan/CE19/SEM_192/Discrete Mathematics/Assignment/Data/file_6.xlsx", 
                     col_types = c("numeric", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text"))
file_21 <- read_excel("D:/quan/CE19/SEM_192/Discrete Mathematics/Assignment/Data/file_21.xlsx", 
                     col_types = c("numeric", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text"))
file_22 <- read_excel("D:/quan/CE19/SEM_192/Discrete Mathematics/Assignment/Data/file_22.xlsx", 
                     col_types = c("numeric", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text"))
file_24 <- read_excel("D:/quan/CE19/SEM_192/Discrete Mathematics/Assignment/Data/file_24.xlsx", 
                     col_types = c("numeric", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text"))

# View(file_6)
# colnames(file_6)
# length(unique(file_6[["Mã số ID"]]))
 


# Change fields' name in dataframe



# Reformat data in dataframe

# for (i in columnNo) {
#   numOfRow <- c(1:length(file_6[i]))
#   for (j in numOfRow) {
#     print(file_6[i,j])
#   }
# }

convertVectorToMatrix = function(file){
  columnNo <- c(1:16)
  columnNames <- c('id','status','startedAt','endedAt','completionTime','totalScore','q1','q2','q3','q4','q5','q6','q7','q8','q9','q10')
  for (i in columnNo){
    colnames(file)[i] <- columnNames[i]
  }
  id <- c()
  status <- c()
  startedAt <- c()
  endedAt <- c()
  completionTime <- c()
  totalScore <- c()
  q1 <- c()
  q2 <- c()
  q3 <- c()
  q4 <- c()
  q5 <- c()
  q6 <- c()
  q7 <- c()
  q8 <- c()
  q9 <- c()
  q10 <- c()
  countLimit = length(file[["id"]])
  count = 0
  for (i in file[["id"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    id <- c(id,i)
  }
  
  # for (i in file[["status"]]){
  #   count = count+1
  #   if (count == countLimit) {
  #     count = 0
  #     break
  #   }
  #   if (i == "Đã hoàn thành"){
  #     status <- c(status,1)
  #   } else if (i == "Chưa bao giờ gửi") {
  #     status <- c(status,0)
  #   }
  # }
  
  for (i in file[["startedAt"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    mydate = as.numeric(strptime(i,format="%d %B %Y  %I:%M %p"))
    startedAt <- c(startedAt,mydate)
  }
  
  for (i in file[["endedAt"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    mydate = as.numeric(strptime(i,format="%d %B %Y  %I:%M %p"))
    endedAt <- c(endedAt,mydate)
  }
  
  for (i in file[["completionTime"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    stringTemp = strsplit(i, " ")
    if ((sapply(stringTemp,length)==2) & stringTemp[[1]][2] == "giây"){
      completionTime <- c(completionTime,as.numeric(stringTemp[[1]][1]))
    } else if ((sapply(stringTemp,length)==2) & stringTemp[[1]][2] == "phút") {
      completionTime <- c(completionTime,as.numeric(stringTemp[[1]][1])*60)
    }
    else{
      completionTime <- c(completionTime,as.numeric(stringTemp[[1]][1])*60 + as.numeric(stringTemp[[1]][3]))
    }
  }
  
  for (i in file[["totalScore"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    totalScore <- c(totalScore,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q1"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-") {
      q1 <- c(q1, 0)
      next
    }
    q1 <- c(q1,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q2"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-"){
      q2 <- c(q2, 0)
      next
    } 
    q2 <- c(q2,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q3"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-"){
      q3 <- c(q3, 0)
      next
    } 
    q3 <- c(q3,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q4"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-"){
      q4 <- c(q4, 0)
      next
    } 
    q4 <- c(q4,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q5"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-"){
      q5 <- c(q5, 0)
      next
    } 
    q5 <- c(q5,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q6"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-"){
      q6 <- c(q6, 0)
      next
    } 
    q6 <- c(q6,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q7"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-"){
      q7 <- c(q7, 0)
      next
    } 
    q7 <- c(q7,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q8"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-"){
      q8 <- c(q8, 0)
      next
    } 
    q8 <- c(q8,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q9"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-"){
      q9 <- c(q9, 0)
      next
    } 
    q9 <- c(q9,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  for (i in file[["q10"]]){
    count = count+1
    if (count == countLimit) {
      count = 0
      break
    }
    if (i == "-"){
      q10 <- c(q10, 0)
      next
    } 
    q10 <- c(q10,as.numeric(gsub(",", ".", gsub("\\.", "", i))))
  }
  
  newDataframe = cbind(id,startedAt, endedAt ,completionTime , totalScore ,
                       q1,q2,q3,q4,q5,q6,q7,q8,q9,q10 )
  result = data.matrix(newDataframe)
  return(result)
}


matrixFile6 = convertVectorToMatrix(file_6)
matrixFile21 = convertVectorToMatrix(file_21)
matrixFile22 = convertVectorToMatrix(file_22)
matrixFile24 = convertVectorToMatrix(file_24)
