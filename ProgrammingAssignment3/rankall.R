## Rankall Function that ranks hospitals in all states by outcome 

## Description: Using outcome of care data for U.S. hospitals this function determines
##      the hospitals of a specified ranking for each state based on the 30 day 
##      mortality rate for a designated outcome 
## Usage: rankall("outcome", num = "best")
## Arguments:    outcome = "heart attack", "heart failure", or "pneumonia"
##              num = "best", "worst", or an integer indicating the rank you would like to see
## Require packages:
## library(dplyr, data.table, magrittr)

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if(sum(state == data$State) < 1) {
                stop("invalid state")
        }
        if(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia") {
        } else {stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        if (outcome == "heart attack") { ## Performs operation based on "heart attack" outcome
                ## Select for "heart attack" outcome creating a data frame with only hospital, 
                ##      state, heartattack columns
                sub <- data %>% select(hospital = 2, state = State, heartattack = 11) %>%
                        ## Replace "Not Available" values with NA's
                        mutate(heartattack=replace(heartattack, heartattack == "Not Available", NA)) %>%                
                        ## Remove rows with NA's for heart attack value
                        filter(!is.na(heartattack)) %>%
                        ## Converts outcome column from character to numeric so it sorts properly
                        mutate(heartattack=as.numeric(heartattack)) %>%
                        ## Sort by state, heartattack, then hospital
                        arrange(state, heartattack, hospital) %>%
                        ## Group by state
                        group_by(state) %>%
                        ## Add rank column that starts count over for every state
                        mutate(rank = row_number(heartattack)) 
        } else if (outcome == "heart failure") {## Performs operation based on "heart failure" outcome
                ## Select for "heart failure" outcome creating a data frame with only hospital, 
                ##      state, heartattack columns
                sub <- data %>% select(hospital = 2, state = State, heartfailure = 17) %>%
                        ## Replace "Not Available" values with NA's
                        mutate(heartfailure=replace(heartfailure, heartfailure == "Not Available", NA)) %>%                
                        ## Remove rows with NA's for heart failure value
                        filter(!is.na(heartfailure)) %>%
                        ## Converts outcome column from character to numeric so it sorts properly
                        mutate(heartfailure=as.numeric(heartfailure)) %>%
                        ## Sort by state, heartfailure, then hospital
                        arrange(state, heartfailure, hospital) %>%
                        ## Group by state
                        group_by(state) %>%
                        ## Add rank column that starts count over for every state
                        mutate(rank = row_number(heartfailure)) 
        } else if (outcome == "pneumonia") {## Performs operation based on "pneumonia" outcome
                ## Select for "pneumonia" outcome creating a data frame with only hospital, 
                ##      state, pneumonia columns
                sub <- data %>% select(hospital = 2, state = State, pneumonia = 23) %>%
                        ## Replace "Not Available" values with NA's
                        mutate(pneumonia=replace(pneumonia, pneumonia == "Not Available", NA)) %>%                
                        ## Remove rows with NA's for pneumonia value
                        filter(!is.na(pneumonia)) %>%
                        ## Converts outcome column from character to numeric so it sorts properly
                        mutate(pneumonia=as.numeric(pneumonia)) %>%
                        ## Sort by state, pneumonia, then hospital
                        arrange(state, pneumonia, hospital) %>%
                        ## Group by state
                        group_by(state) %>%
                        ## Add rank column that starts count over for every state
                        mutate(rank = row_number(pneumonia)) 
        }
        ## Generates data frame with selected rank for each state based on num
        ranked <- if (num == "best") {
                sub[ sub$rank == ave(sub$rank, sub$state, FUN=min), ]
        } else if (num == "worst") {
                sub[ sub$rank == ave(sub$rank, sub$state, FUN=max), ]
        } else { sub[ sub$rank == num, ] }
        ## Selects only columns hospital and state 
        ranked <- data.table(select(ranked, hospital, state))
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        ## Create "blanks" data frame to combine with "ranked" to provide NA values for
        ##      state levels that were dropped in the above subset
        states <- unique(sub$state)
        blanks <- data.frame(states)                 
        blanks <- blanks %>% mutate(hospital = NA, state = states) 
        blanks <- select(blanks, hospital, state)
        ## Combine "states" and "blanks" data frames to provide NA values for state
        ##      levels that were dropped
        answer <- data.frame(rbind(ranked, blanks) %>% setkey(state) %>% unique)
        ## Change state from factor to character so it can be sorted
        answer <- data.frame(lapply(answer, as.character))                
        ## Sort into alphabetical order
        sorted <- answer[order(answer$state), ]
        sorted        
}