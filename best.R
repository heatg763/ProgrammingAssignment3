best<- function(state, outcome){
  reason<- as.character(outcome)
  state<- as.character(state)
  ## get data
  outcome<- read.csv("outcome-of-care-measures.csv", colClasses = ("character"))
  
  ## test for validity of arguements
  stlst<- as.vector(unique(outcome$State))
  outlst<- c("heart attack", 'heart failure', "pneumonia")
  if(is.na(match(state, stlst))) stop("invalid state")
  if(is.na(match(reason, outlst))) stop("invalid outcome")
  
  ## split dataframe
  n<- 0
  if (reason == "heart attack") {
    n<- 11
  } else if (reason == "heart failure") {
    n<- 17
  } else {
    n<- 23
  }
  
  options(warn=-1)
  valid<- data.frame(outcome[,2], outcome[,7], as.numeric(outcome[,n]))
  options(warn=0)
  specout<- split(valid, valid[,2])
  stateout<- data.frame(specout[state])
  
  ## Order data frame by death rate then alpabetically by hospital
  finalout<- stateout[order(stateout[,3], stateout[,1]),]
  desiredhos<- as.vector(as.matrix(finalout[1,1]))
  desiredhos
}

