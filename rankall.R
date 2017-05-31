rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomedat <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    
    
    ## Check that outcome is valid
    outcome <- tocamel(outcome, upper = TRUE, sep = ".")
    outcomeslist <- c("Pneumonia", "Heart.Failure", "Heart.Attack")
    if (!(outcome %in% outcomeslist)){
        stop("invalid outcome")
    }
    
    tablehead <- paste("Hospital.30.Day.Death..Mortality..Rates.from", outcome, sep = ".")
    
    ## For each state, find the hospital of the given rank
    
    
    hospitals <- character()
    
    for (i in unique(outcomedat$State)){
        statedata <- outcomedat[outcomedat$State == i, ]
        statedata <- statedata[complete.cases(statedata[[tablehead]]), ]
        
        if (num == "best"){
            num <- 1
        } else if (num == "worst") {
            num <- nrow(statedata)
        }
                
        arrangedstatedata <- arrange(statedata, statedata[[tablehead]], statedata["Hospital.Name"])
        arrangedstatedata$Rank <- NA
        arrangedstatedata$Rank <- row_number(arrangedstatedata[[tablehead]])

        hospital <- arrangedstatedata[which(arrangedstatedata$Rank == num), "Hospital.Name"]
        if (length(hospital) == 0){
            hospital <- NA
        }
        
        hospitals <- append(hospital, hospitals)
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    rankedhospital <- data.frame(Hospital = hospitals,state =  unique(outcomedat$State))
    rankedhospital
}