pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  
  ## Return vector of formated text based on id
  filenames <- sprintf("%03d.csv", id)
  ## Paste all the files together
  filenames <- paste(directory, filenames, sep="/")
  ## Apply read.csv to all files
  ldataframep<- lapply(filenames, read.csv)
  ## Combine results in dataframe
  dataframep=ldply(ldataframep)
  ## Take mean of dataframe with no NA
  pollutionmean <- mean(dataframep[, pollutant], na.rm = TRUE)
  ## Return Mean
  pollutionmean
  
}