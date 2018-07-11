#### ----------  first function

best <- function(state = 'SC', outcome_s = 'heart attack') {
  ## Read outcome data
  outcome <- read.csv('rprogdataProgAssignment3-data/outcome-of-care-measures.csv', colClasses = "character")
  
  ## Check that state and outcome are valid
  if (sum(state == unique(outcome$State)) != 1 ){
    stop(" invalid state")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  tmp = outcome[outcome$State==state,]
  #print('SC')
  if (identical(outcome_s,'heart attack')) {
    TRUE
    a = tmp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  } else if (identical(outcome_s,'pneumonia')){
    a = tmp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  } else if (identical(outcome_s,'heart failure')){
    a = tmp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  } else {
    stop('invalid outcome')
  }
  
  if (sum(as.numeric(a) == min(as.numeric(a), na.rm = TRUE),na.rm = TRUE) == 1){
    print(TRUE)
    hosp_ind = which(as.numeric(a) == min(as.numeric(a), na.rm = TRUE))
    tmp$Hospital.Name[hosp_ind]}
  else {print(FALSE)}
}

best('SC','ge')
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")


best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
