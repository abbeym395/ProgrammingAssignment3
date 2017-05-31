library(rapportools)
library(datasets)
library(plyr)
library(dplyr)

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomedat <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    ## Check that state and outcome are valid
    states <- unique(outcomedat$State)
    if (!(state %in% states)) {
        stop("invalid state")
    }
    
    outcome <- tocamel(outcome, upper = TRUE, sep = ".")
    outcomeslist <- c("Pneumonia", "Heart.Failure", "Heart.Attack")
    if (!(outcome %in% outcomeslist)){
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    tablehead <- paste("Hospital.30.Day.Death..Mortality..Rates.from", outcome, sep = ".")
    
    outcomedat <- outcomedat[complete.cases(outcomedat[[tablehead]]), ]
    arrangeddata <- arrange(outcomedat, outcomedat[[tablehead]], outcomedat["Hospital.Name"])
    arrangeddata <- arrangeddata[arrangeddata$State == state, ]
    arrangeddata$Rank <- NA
    arrangeddata$Rank <- row_number(arrangeddata[[tablehead]])
    
    if (num == "best"){
        num <- 1
    } else if (num == "worst") {
        num <- max(arrangeddata$Rank)
    }
    
    bestentry <- arrangeddata[which(arrangeddata$Rank == num), ]
    bestentry$Hospital.Name[1]
}