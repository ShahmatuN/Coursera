state <- "TX"
outcome <- "heart attack"
directory <- "./outcome-of-care-measures.csv"
data <- read.csv(directory, colClasses="character")
data[, 11] <- as.numeric(data[, 11]) # heart attack
data[, 17] <- as.numeric(data[, 17]) # heart failure
data[, 23] <- as.numeric(data[, 23]) # pneumonia
outcomes <- c("heart attack", "heart failure", "pneumonia")

hospFind <- function (data, column, state)
{
  tempData <- (data[data$State == state & !is.na(data[, column]), 2])
  TempData <- cbind(TempData,(data[data$State == State & !is.na(data[, column]), column]) )
  hosp <- TempData[ TempData[, 2] == min(TempData[, 2]), 1]
  return(hosp)
}

if 
  (!State %in% data$State) 
  {
    stop ("invalid state")
  }
else 
  {
    if 
      (!outcome %in% outcomes) 
    {
        stop ("invalid outcomes")
    }
    else
    {
      if(outcome == "heart attack") 
        {
          hosp <- hospFind(data, 11, state)
        } 
      else 
          if(outcome == "heart failure") 
            {
            hosp <- hospFind(data, 17, state)
            } 
          else 
            {
            hosp <- hospFind(data, 23, state)
            }
      
      return(hosp)
    }
      
  
}
tempData <- na.omit(data)
colnum <- 11
tempData <- cbind(tempData,(data[data$State == State & na.omit(data[, 11]), 11]) )
min(data [ ,11], na.rm = T)
 y <- data[data[, 11] == 10.1 & data$State == State, 2]
y<- min(x[, 2])
y <- y[!is.na(y)]
hosp1 <- as.character( tempData[ tempData[, 2] == min(tempData[, 2]), 1])
state_subset <- data[data[, 7]==state, ]
outcome_arr <- state_subset[state_subset[,11] == min(state_subset[, 11]),2 ]
