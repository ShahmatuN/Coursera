source("rankHospital.R")
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  directory <- "./outcome-of-care-measures.csv"
  data <- read.csv(directory, colClasses="character")
  data[, 11] <- suppressWarnings(as.numeric(data[, 11])) # heart attack
  data[, 17] <- suppressWarnings(as.numeric(data[, 17])) # heart failure
  data[, 23] <- suppressWarnings(as.numeric(data[, 23])) # pneumonia
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  allStates <- sort(unique(data [,7]))
  hospital <-allStates
  for (i in 1:length(allStates))
  {
    hospital[i] <- rankhospital(allStates[i],outcome,num)
    
  }
  dataHospitals <- data.frame(hospital = hospital, state = allStates)
  return (dataHospitals)

  }



