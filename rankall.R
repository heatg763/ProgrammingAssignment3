rankall<- function(outcome, num = "best") {
  reason<- as.character(outcome)
  originalnum<- num
  
  ## get data
  outcome<- read.csv("outcome-of-care-measures.csv", colClasses = ("character"))
  
  # code cause
  n<- 0
  if (reason == "heart attack") {
    n<- 11
  } else if (reason == "heart failure") {
    n<- 17
  } else {
    n<- 23
  }
  
  ## get vector of states and hospitals for rank
  stlst<- as.vector(sort(unique(outcome$State)))
  vecthos<- rep(0,length(stlst))
  
  for (i in 1:length(stlst)) {
    ## test for validity of arguements
    num<- originalnum
    state<- stlst[i]
    outlst<- c("heart attack", 'heart failure', "pneumonia")
    if(is.na(match(state, stlst))) stop("invalid state")
    if(is.na(match(reason, outlst))) stop("invalid outcome")
    
    ## split dataframe
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
    
    vecthos[i]<- as.character(as.matrix(finalout[num,1]))
    
  }
  
  final<- data.frame(hospital = vecthos, state = stlst)
  final
  
}