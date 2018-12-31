library(dplyr)
library(magrittr)
library(stringr)

# Reshape and expand
reshapeForLSTM <- function(originalDF, numTimeSteps, columnsToExclude) {

  originalColumnsToExclude <- columnsToExclude
  
  # Create new columns based on previous steps
  for (t in 1:(numTimeSteps)) {
    
    originalDF %<>%
      mutate_at(vars(-one_of(columnsToExclude)), funs(lagTmp = lag(., n = t))) %>%
      rename_at(vars(contains("lagTmp")), ~str_replace_all(., "lagTmp", as.character(t)))
    
    columnsToExclude <- c(columnsToExclude, colnames(originalDF)[str_detect(colnames(originalDF), "_")])
  }
  
  res_matrix <- as.matrix(select(originalDF, -originalColumnsToExclude))
  
  res_matrix <- res_matrix[complete.cases(res_matrix),]
  
  dim(res_matrix) <- c(nrow(res_matrix),numTimeSteps+1,ncol(res_matrix)/(numTimeSteps+1))
  
  return (originalDF)
}

reshapeForLSTMLoop <- function(originalDF, numTimeSteps, columnsToExclude) {
  
  originalDF <- as.data.frame(originalDF)
  
  colNames <- colnames(originalDF)
  newMatrix <- as.matrix(originalDF[,!colNames %in% columnsToExclude])
  
  for (i in 1:ncol(originalDF)) {
    
    thisColName <- colNames[i]
    
    if (thisColName %in% columnsToExclude) {
      next
    }
    
    laggedColumns <- sapply(1:numTimeSteps, function(x) {
      c(rep(NA, x), head(originalDF[,i], -x))
    })
    
    colnames(laggedColumns) <- paste0(thisColName, "_", 1:numTimeSteps)
    
    newMatrix <- cbind(newMatrix, laggedColumns)
  }
  
  # Remove NAs
  newMatrix <- newMatrix[complete.cases(newMatrix),]
  
  dim(newMatrix) <- c(nrow(newMatrix),numTimeSteps+1,ncol(newMatrix)/(numTimeSteps+1))
  return (newMatrix)
}
