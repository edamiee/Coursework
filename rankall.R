##Damien Edwards
## Programming Assignment 3
options(warn=-1)
rankall <- function(outcome, num = "best") {
  outcomec <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state <- outcomec$State
  state <- sort(unique(state))
  hospital <- rep("", length(state))
  
  #Loop through to check outcomes
  for (i in 1:length(state)) {
    state_list<- outcomec[outcomec$State==state[i],]
    if (outcome == 'heart attack') {
      sub_set <- as.numeric(state_list[,11])
    } else if (outcome == 'heart failure') {
      sub_set <- as.numeric(state_list[,17])
    } else if (outcome == 'pneumonia') {
      sub_set <- as.numeric(state_list[,23])
    } else {
      stop("invalid outcome")
    }
    
    Rank_list <- rank(sub_set, na.last=NA)
    if (num=="best") {
      r <- 1
    } else if (num =="worst") {
      r <- length(Rank_list)
    } else if (num <= length(Rank_list) ) {
      r <- num
    } else {
      r <- NA
    }
    
    if (is.na(r)) {
      hospital[i] <- NA
    } else {
      hospital[i] <- state_list$Hospital.Name[order(sub_set, state_list$Hospital.Name)[r]]
    }
    
  }
  
  return(data.frame(hospital=hospital, state=state))
}