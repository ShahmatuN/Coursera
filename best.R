
hospFind <- function (data, column, state){
  TempData <- data[data[,7] == state, ]
  hosp <- na.omit(TempData[TempData[ ,column] == min(TempData[, column], na.rm = T), 2])
  hosp <- hosp[!is.na(hosp)]
  return(hosp)
}
best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  #State <- "MD"
  #outcome <- "heart attack"
  directory <- "./outcome-of-care-measures.csv"
  data <- read.csv(directory, colClasses="character")
  data[, 11] <- suppressWarnings(as.numeric(data[, 11])) # heart attack
  data[, 17] <- suppressWarnings(as.numeric(data[, 17])) # heart failure
  data[, 23] <- suppressWarnings(as.numeric(data[, 23])) # pneumonia
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  
  
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% outcomes) {
    stop("invalid outcome")
  } else {
    if(outcome == "heart attack") {
      hospName <- hospFind(data, 11, state)
    } else if(outcome == "heart failure") {
      hospName <- hospFind(data, 17, state)
    } else {
      hospName<- hospFind(data, 23, state)
    }
    return(hospName)
  }
}


best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
