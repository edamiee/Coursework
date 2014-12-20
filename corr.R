##Damien Edwards
## Part 3

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## List of files of csv type
  file_list<-list.files(directory,pattern = '\\.csv')
  
  Correl_collect<- vector(mode="numeric")
  Correl_collect <- c()
  ## Loop thru files in file list
  for(i in file_list){
    
    ## Read the csv values
    data_frame <- read.csv(paste(directory, "/", i, sep =""))
    
    ## Completed Cases
    Completed <- complete.cases(data_frame)
    data_frame<-data_frame[Completed,] 
    ## Sum of completed cases with NA omitted
    Sum_completed <- sum(Completed)
     
   
      if (Sum_completed >threshold){
        ## Data frame with completed cases
          ## Correlation function
        Correl_collect <-c(Correl_collect,cor(data_frame$sulfate,data_frame$nitrate)) 
        ## print(Correl_collect)
       
      }
      ##else return(numeric())
    

  }  
     
return(Correl_collect)  
    
  }