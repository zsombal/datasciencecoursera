#### ----------  second function

rankhospital <- function(state, outcome_s, num = "best") {
  ## Read outcome data
  outcome <- read.csv('rprogdataProgAssignment3-data/outcome-of-care-measures.csv', colClasses = "character")
  
  #state = 'TX'
  #outcome_s = "heart failure"
  ## Check that state and outcome are valid
  if (sum(state == unique(outcome$State)) != 1 ){
    stop(" invalid state")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  tmp = outcome[outcome$State==state,]
  #print('SC')
  if (identical(outcome_s,'heart attack')) {
    TRUE
    a = tmp[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack','Hospital.Name')]
  } else if (identical(outcome_s,'pneumonia')){
    a = tmp[,c('Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia','Hospital.Name')]
  } else if (identical(outcome_s,'heart failure')){
    a = tmp[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure','Hospital.Name')]
  } else {
    stop('invalid outcome')
  }
  
  # get rid of all the NA values
  a = a[!is.na(as.numeric(a[,1])),]
  a[,1] = as.numeric(a[,1])
  
  # order hospitals by rank
  a_ord = a[order(a[,1], a[,2]),]
  
  if (identical(num,'worst')) {
    a_ord[dim(a_ord)[1],2]
  } else if (identical(num,'best')){
    a_ord[1,2]
  } else {
    a_ord[num,2]
  }
  
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
