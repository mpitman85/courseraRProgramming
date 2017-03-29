## Function that finds the best hopsital in a state given a particular outcome

## Description: Using outcome of care data for U.S. hospitals this function determines
##      the hospital with the lowest 30 day mortality rate for a designated outcome in the 
##      state specified.
## Usage: best("state", "outcome")
## Arguments:    state = 2 character abbreviated name for a state
##              outcome = "heart attack", "heart failure", or "pneumonia"
## Required Packages:
##      library(dplyr)

best <- function(state, outcome) {
        ##read data in
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##check that state and outcome are valid
        if(sum(state == data$State) < 1) {
                stop("invalid state")
        }
        if(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia") {
        } else {stop("invalid outcome")
        }
        
        ##return hospital name in that state with lowest 30 day death
        subset <- filter(data, State==state) %>%
                select(2, heartattack = 11, heartfailure = 17, pneumonia = 23) %>%
                mutate(heartattack = as.numeric(heartattack), 
                       heartfailure = as.numeric(heartfailure),
                       pneumonia = as.numeric(pneumonia))
        if (outcome == "heart attack") { 
                ordered <- arrange(subset, heartattack, Hospital.Name)
        } else if (outcome == "heart failure") {
                ordered <- arrange(subset, heartfailure, Hospital.Name)
        } else if (outcome == "pneumonia") {
                ordered <- arrange(subset, pneumonia, Hospital.Name)
        }
        return <- ordered[1,1]
        return
}