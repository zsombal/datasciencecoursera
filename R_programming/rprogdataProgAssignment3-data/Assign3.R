
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


#### ----------  third function

rankall <- function(outcome_s, num = "best") {
  ## Read outcome data
  outcome <- read.csv('rprogdataProgAssignment3-data/outcome-of-care-measures.csv', colClasses = "character")
  
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  if (identical(outcome_s,'heart attack')) {
    TRUE
    a = outcome[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack','Hospital.Name','State')]
  } else if (identical(outcome_s,'pneumonia')){
    a = outcome[,c('Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia','Hospital.Name','State')]
  } else if (identical(outcome_s,'heart failure')){
    a = outcome[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure','Hospital.Name','State')]
  } else {
    stop('invalid outcome')
  }
  
  # get rid of all the NA values
  a = a[!is.na(as.numeric(a[,1])),]
  a[,1] = as.numeric(a[,1])
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  st_ls = c()
  hosp_list = c()
  for (ss in unique(a[,3])[order(unique(a[,3]))]){
    print(ss)
    a1 = subset(a,a[,3]==ss)
    a1_ord = a1[order(a1[,1],a1[,2]),]
    st_ls =c(st_ls, ss)
    
    if (identical(num,'worst')) {
      hosp_list = c(hosp_list,a1_ord[dim(a1_ord)[1],2])
    } else if (identical(num,'best')){
      hosp_list = c(hosp_list,a1_ord[1,2])
    } else {
      hosp_list = c(hosp_list,a1_ord[num,2])
    }
  }
  
  out <- as.data.frame(list(hospital=hosp_list, state=st_ls))
  rownames(out) <- st_ls
  out
  
  
}
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)


