##Damien Edwards
## part 2

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
  
  ## Initialize vector
  z <- vector()
  ## Loop through vector
  for(i in 1:length(id)){
    x <- id
    ## Set z equal to the Sum of complete cases
    if(id[i]<10){
    z <- c(z, sum(complete.cases(read.csv(as.character(paste0(getwd(),"/",directory,"/",pattern = "00",id[i],".csv"))))))
   } else if (id[i]>=10 && id[i]<100){
        z <- c(z, sum(complete.cases(read.csv(as.character(paste0(getwd(),"/",directory,"/",pattern = "0",id[i],".csv")))))) 
      } else{
        z <- c(z, sum(complete.cases(read.csv(as.character(paste0(getwd(),"/",directory,"/",id[i],".csv")))))) 
      }
         }
  ##Create data frames
  dataframep <- data.frame(x, z)
  ## Define column names
  colnames(dataframep) <- c("id","nobs")
  ## Return data frame complete cases
  return(dataframep)
}