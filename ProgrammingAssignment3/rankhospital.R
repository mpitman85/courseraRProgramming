## Rank Hospital Function that ranks hospitals by outcome in their state

## Description: Using outcome of care data for U.S. hospitals this function determines
##      the ranking for a hospital based on the 30 day mortality rate for a designated outcome 
##      in the state specified.
## Usage: rankhospital("state", "outcome", num = "best")
## Arguments:    state = 2 character abbreviated name for a state
##              outcome = "heart attack", "heart failure", or "pneumonia"
##              num = "best", "worst", or an integer indicating the rank you would like to see
## Required Packages:
##      library(dplyr)

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if(sum(state == data$State) < 1) {
                stop("invalid state")
        }
        if(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia") {
        } else {stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30 day death rate
        subset <- data %>%
                filter(State==state) %>%
                select(2, heartattack = 11, heartfailure = 17, pneumonia = 23) %>%
                mutate(heartattack=replace(heartattack, heartattack == "Not Available", NA)) %>%
                mutate(heartfailure=replace(heartfailure, heartfailure == "Not Available", NA)) %>%
                mutate(pneumonia=replace(pneumonia, pneumonia == "Not Available", NA)) %>%
                mutate(heartattack=as.numeric(heartattack), 
                       heartfailure=as.numeric(heartfailure),
                       pneumonia=as.numeric(pneumonia))
        if (outcome == "heart attack") { 
                ordered <- filter(subset, !is.na(heartattack)) %>%
                        arrange(heartattack, Hospital.Name)
        } else if (outcome == "heart failure") {
                ordered <- filter(subset, !is.na(heartfailure)) %>%
                        arrange(heartfailure, Hospital.Name)
        } else if (outcome == "pneumonia") {
                ordered <- filter(subset, !is.na(pneumonia)) %>%
                        arrange(pneumonia, Hospital.Name)
        }
        return <- if (num == "best") {
                ordered[1,1]
        } else if (num == "worst") {
                ordered[nrow(ordered),1]
        } else if (num > nrow(ordered)) {
                stop("NA")
        } else {ordered[num, 1]
        }
        return
}