
#Coursera Programming Assignment 3
#R Programming
#4.Ranking hospitals in all states


rankall <- function(outcome,num){
  outcome <- outcome
  # read the csv file from working directorey
  outcome_file <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  # creating state vector
  state_seq <- unique(outcome_file$State)
  
  #selecting the required columns  from the dataset
  outcome_short <- outcome_file[c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  # rename the columns for convenience 
  names(outcome_short)<- c("Hospital.Name","State","heart attack","heart failure","pneumonia")
  
  #supress the warning while changing the class of the column to numeric & NA has removed
  suppressWarnings(outcome_short[,c(3:5)] <- sapply(outcome_short[,c(3:5)],as.numeric))
  outcome_clean<- outcome_short[complete.cases(outcome_short),]
  
  #creating the vector
  
  output <- vector()
  #for each State in state_seq ;reorder the outcome and hospital then a per the "num" provide the index of the data
  for( i in 1:length(state_seq)){
    state_data<- outcome_clean %>% filter(outcome_clean$State == state_seq[i])
    state_outcome <- state_data[,c('Hospital.Name','State',paste0(outcome))]
    state_outcome_order <- state_outcome[order(state_outcome[,3],state_outcome[,1]),]
    
    hospitals <- if( num == "best"){
      state_outcome_order[1,1]
    }else if(num == "worst"){
      state_outcome_order[nrow(state_outcome_order),1]
    }else{
      state_outcome_order[num,1]
    }
    
    output <- append(output, c(hospitals, state_seq[i]))
    
  }
  output <- as.data.frame(matrix(output, length(state_seq), 2, byrow = TRUE))
  colnames(output) <- c("hospital", "state")
  rownames(output) <- state_seq
  
  output
  
}

####example

#tail(rankall("pneumonia", "worst"), 3)
