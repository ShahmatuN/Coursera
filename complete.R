complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  k <- 1
  nobs<-matrix(,length(id),2)
    if(grep("specdata", directory) == 1) 
  {
    directory <- ("./specdata/")
  }
  allFiles <- as.character( list.files(directory) )
  filePaths <- paste(directory, allFiles, sep="")
  for(i in id) 
  {
    currentFile <- read.csv(filePaths[i], header=T, sep=",")
    head(currentFile)
    naRemoved <- currentFile[!is.na(currentFile[, "sulfate"]), "sulfate"]
    nobs[k,1]<- i
    nobs[k,2] <-length(naRemoved)
    k <-k+1
  }
return(nobs)
}
complete("specdata", c(2, 4, 8, 10, 12))

