corr <- function(directory, threshold = 0) 
  {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
 
  ## directory <- "specdata" 
  vectorCorrelation <-as.numeric()
  k <- 1
  if(grep("specdata", directory) == 1) 
  {
    directory <- ("./specdata/")
  }
  allFiles <- as.character( list.files(directory) )
  filePaths <- paste(directory, allFiles, sep="")
  
  allSamples <- complete(directory = directory)
  ids <- allSamples$id[allSamples$nobs > threshold]
  for ( i in ids)
  {
    
    currentFile <- read.table(filePaths[i], header=T, sep=",")
    allData <- na.omit(currentFile)
    vectorCorrelation[k] <-(cor(allData$nitrate,allData$sulfate))
    k <- k+1
  }
  return(vectorCorrelation)
}

