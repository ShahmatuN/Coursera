setwd("C:/Users/zhiltsov/Desktop/Coursera/sha-1/Assigment 3")
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  #state <- "TX"
  #outcome <- "heart attack"
  directory <- "./outcome-of-care-measures.csv"
  data <- read.csv(directory, colClasses="character")
  data[, 11] <- suppressWarnings(as.numeric(data[, 11])) # heart attack
  data[, 17] <- suppressWarnings(as.numeric(data[, 17])) # heart failure
  data[, 23] <- suppressWarnings(as.numeric(data[, 23])) # pneumonia
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  hospFindRating <- function (data, numCol, state, numRate)
{
    decreas <- as.logical(F)
    dataState <-data[data [, 7] == state, ]
    
    if (numRate ==  "best"){
      numRate <- 1
      decreas <- as.logical(F)
    }
    if (numRate == "worst"){
      numRate <- 1
      decreas <- as.logical(T)
    }
    if (numRate > length(sort(dataState[, 11]))) { return(NA)}
    Sorted <- dataState[ order(dataState[,numCol],dataState[,2],decreasing = decreas),] 
    hospNAme <- Sorted[numRate,2]
    return (hospNAme)
}
  
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% outcomes) {
    stop("invalid outcome")
  } else {
    if(outcome == "heart attack") {
      hospName <- hospFindRating(data, 11, state, num)
    } else if(outcome == "heart failure") {
      hospName <- hospFindRating(data, 17, state, num)
    } else {
      hospName<- hospFindRating(data, 23, state, num)
    }
    return(hospName)
  }
}
rankhospital("TX", "heart failure", 5000)

