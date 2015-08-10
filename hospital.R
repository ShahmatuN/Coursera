State <- "MD"
outcome <- "heart attack"
directory <- "./outcome-of-care-measures.csv"
data <- read.csv(directory, colClasses="character")
data[, 11] <- as.numeric(data[, 11]) # heart attack
data[, 17] <- as.numeric(data[, 17]) # heart failure
data[, 23] <- as.numeric(data[, 23]) # pneumonia
outcomes <- c("heart attack", "heart failure", "pneumonia")

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
      
  
  }
x <- (data[data$State == State & !is.na(data[, 11]), 2])

x <- cbind(x,(data[data$State == State & !is.na(data[, 11]), 11]) )
min(data [ ,11], na.rm = T)
y <- data[data[, 11] == 10.1 & data$State == State, 2]
y <- y[!is.na(y)]
