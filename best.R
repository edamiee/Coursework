##Damien Edwards
## Assignment 3

best <- function(state, outcome) {
  ## Read outcome data
  
  outcomec <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 
  outcomec[,11] <- suppressWarnings(as.numeric(outcomec[,11]))
  outcomec[,17] <- suppressWarnings(as.numeric(outcomec[,17]))
  outcomec[,23] <- suppressWarnings(as.numeric(outcomec[,23]))
  #create a list of states for verification
  state_check <- unique(outcomec$State)
  ## Check that state and outcome are valid
  condition_check <- c('heart attack', 'heart failure', 'pneumonia')
  if (!state %in% state_check) { stop('invalid state') }
  if (!outcome %in% condition_check) { stop('invalid outcome') }
   ## Return hospital name in that state with lowest 30-day death
  ## rate
  #make a subset, just for that state
  sub_set <- outcomec[grep(state, outcomec$State, ignore.case=T),]
  
  #pick a column to select based on the outcome
  if (outcome == 'heart attack' ) { selector <- 11 }
  if (outcome == 'heart failure' ) { selector <- 17 }
  if (outcome == 'pneumonia' ) { selector <- 23 }
  
  sort_set <- sub_set[order(sub_set[,selector],sub_set[,2]),]
  sort_set[1,2]
  
  
  
  
}