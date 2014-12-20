##Damien Edwards
## Programming Assignment 3
options(warn=-1)
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomec <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  sub_set <- outcomec[outcomec$State==state,]
  ## Check that state and outcome are valid
  state_check <- unique(outcomec$State)
  condition_check <- c('heart attack', 'heart failure', 'pneumonia')
  if (!state %in% state_check) { stop('invalid state') }
  if (!outcome %in% condition_check) { stop('invalid outcome') }
  #pick a column to select based on the outcome
  if (outcome == 'heart attack' ) { selector<- as.numeric(sub_set[,11]) }
  if (outcome == 'heart failure' ) {  selector<- as.numeric(sub_set[,17]) }
  if (outcome == 'pneumonia' ) {  selector<- as.numeric(sub_set[,23]) }
 
  outcomec <- rank(selector, na.last=NA)
  
  if (num=="best") {
    rankhosp <- 1
  } else if (num =="worst") {
    rankhosp <- length(outcomec)
  } else if (num <= length(outcomec) ) {
    rankhosp <- num
  } else {
    return(NA)
  }
   
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  return(sub_set$Hospital.Name[order(selector, sub_set$Hospital.Name)[rankhosp]])

}