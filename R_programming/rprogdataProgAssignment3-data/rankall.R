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
