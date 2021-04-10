
#Coursera Programming Assignment 3
#R Programming
#2.Finding the best hospital in a state



best<- function(state,outcome){
  
  # read the csv file from working directorey
  outcome_file<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  # state & outcome input validation
  state_seq <- (unique(outcome_file$State))
  if(!state %in% state_seq){
    stop("invalid state")}
  if( !outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  
  # selecting the required columns from the dataset
  outcome_short <- outcome_file[c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  
  # To change the class of the columns to numeric
  outcome_short[,c(3:5)]<-sapply(outcome_short[,c(3:5)], as.numeric)
  # filter the State variable and remove the NA's from the data
  outcome_state <- outcome_short %>% filter(outcome_short$State == state)
  outcome_state <- outcome_state[complete.cases(outcome_state),]
  # conditions on the basis of outcomes
  if(outcome == "heart attack"){
    outcome_dt <-outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    outcome_name <- outcome_dt[,"Hospital.Name"][1]
  }else if(outcome == "heart failure"){
    outcome_dt <-outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    outcome_name <- outcome_dt[,"Hospital.Name"][1]
  }else if(outcome == "pneumonia"){
    outcome_dt <-outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    outcome_name <- outcome_dt[,"Hospital.Name"][1]
  }
  return(outcome_name)
}
#####example##########
#best(state ="AL",outcome = "heart failure")
#best("TX", "heart attack")
#best("TX", "heart failure")
#best("BB", "heart attack")
#best("NY", "hert attack")