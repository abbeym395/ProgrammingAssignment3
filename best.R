## A fuunction to determine the best hospital in a given state, based
## on the provided outcome

library(rapportools)

testcase <- function(){
    states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
    for (i in states){
        best(i, "heart failure")
    }
}

best <- function(state, outcome) {
    ## Read outcome data
    options(stringsAsFactors = FALSE)
    outcomedat <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
    
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    tablehead <- paste("Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "")
    outcomedat <- sort_df(outcomedat, vars = "Hospital.Name")
    print(head(outcomedat))
    outcomedat <- outcomedat[outcomedat$State == state, ]
    
    outcomedatcol <-outcomedat[[tablehead]]
    outcomedatcol <- as.numeric(outcomedatcol)
    
    outcomedatcol <- na.omit(outcomedatcol)
    maximums <- which(outcomedatcol == max(outcomedatcol))
    
    print(outcomedat$Hospital.Name[maximums])
    
    
}
