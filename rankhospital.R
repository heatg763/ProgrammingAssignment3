rankhospital<- function(state, outcome, num){
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
  statena<- data.frame(specout[state])
  save<- complete.cases(stateout)
  stateout<- statena[save,]
  finalout<- stateout[order(stateout[,3], stateout[,1]),]
  
  finalout$rank <- NA
  finalout$rank[order(finalout[,3])] <- 1:nrow(finalout)
  
  ## Determin desired rank
  if (num == "best") {
    num<- 1
  } else if (num == "worst") {
    num<- nrow(finalout)
  } else { num<- num }
  
  print(as.vector(as.matrix(finalout[num,1])))
}