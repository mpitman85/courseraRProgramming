setwd("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/ProgrammingAssignment3")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
str(outcome)
library(dplyr)

## 1. Plot the 30 day mortality rates for heart attack
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11]) ## Histogram of the 30 day mortality rate for heart attack

## 2. Finding the best hospital in a state
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
        
##Working on 2:
##getting state and outcome input correct
state <- "NY"
state != data$State
ifelse (sum(state == data$State) < 1, print("Error"), print("Did not work"))
sum(state==data$State)  
outcome <- "pneumonia"
outcome != "heart attack" | outcome != "heart failure" | outcome != "pneumonia"

##Find best AL hospital for heart attack
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
state <- "AL"
outcome <- "pneumonia"
subset <- filter(data, State==state) %>%
        select(2, heartattack = 11, heartfailure = 17, pneumonia = 23)
if (outcome == "heart attack") { 
        ordered <- arrange(subset, heartattack, Hospital.Name)
} else if (outcome == "heart failure") {
        ordered <- arrange(subset, heartfailure, Hospital.Name)
} else if (outcome == "pneumonia") {
        ordered <- arrange(subset, pneumonia, Hospital.Name)
}
ordered   
return <- ordered[1,1]
return

##Testing 2
best("AL", "heart attack")
best("KY", "pneumonia")
best("KY", "heart attack")
best("TI", "heart attack")
best("AL", "heartattack")
best("SC", "heart attack")
best("SC", "pneumonia")
best("SC", "heart failure")

##3. Ranking hospitals by outcome in state
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

##Working on 3.
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
state <- "AL"
outcome <- "heart attack"
num <- "best"
subset <- data %>%
        filter(State==state) %>%
        select(2, heartattack = 11, heartfailure = 17, pneumonia = 23) %>%
        mutate(heartattack=replace(heartattack, heartattack == "Not Available", NA)) %>%
        mutate(heartfailure=replace(heartfailure, heartfailure == "Not Available", NA)) %>%
        mutate(pneumonia=replace(pneumonia, pneumonia == "Not Available", NA))
       
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

rank <- 100
return <- if (rank == "best") {
        ordered[1,1]
        } else if (rank == "worst") {
                ordered[nrow(ordered),1]
        } else if (rank > nrow(ordered)) {
                stop("NA")
        } else {ordered[rank, 1]
        }
return

## Testing 3.
rankhospital("AL", "heart attack")
rankhospital("AL", "heart attack", "worst")        
rankhospital("AL", "heart attack", 5)
rankhospital("AL", "heart attack", 100)

## 4. Ranking Hopitals in All States
install.packages("data.table")
install.packages("magrittr")
library(data.table)
library(magrittr)
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


## Working on 4

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome <- "heart attack"
num <- 4
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
        ## Generates data frame with selected rank for each state based on num
        ranked <- if (num == "best") {
                subset[ subset$rank == ave(subset$rank, subset$state, FUN=min), ]
                } else if (num == "worst") {
                 subset[ subset$rank == ave(subset$rank, subset$state, FUN=max), ]
                } else { subset[ subset$rank == num, ] }
        ## Selects only columns hospital and state 
        ranked <- data.table(select(ranked, hospital, state))
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
        ## Generates data frame with selected rank for each state based on num
        ranked <- if (num == "best") {
                subset[ subset$rank == ave(subset$rank, subset$state, FUN=min), ]
        } else if (num == "worst") {
                subset[ subset$rank == ave(subset$rank, subset$state, FUN=max), ]
        } else { subset[ subset$rank == num, ] }
        ## Selects only columns hospital and state 
        ranked <- data.table(select(ranked, hospital, state))
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
        ## Generates data frame with selected rank for each state based on num
        ranked <- if (num == "best") {
                subset[ subset$rank == ave(subset$rank, subset$state, FUN=min), ]
        } else if (num == "worst") {
                subset[ subset$rank == ave(subset$rank, subset$state, FUN=max), ]
        } else { subset[ subset$rank == num, ] }
        ## Selects only columns hospital and state 
        ranked <- data.table(select(ranked, hospital, state))
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

## Testing 4.

rankall("heart attack", num="best")
rankall("heart failure", num="best")             
rankall("pneumonia", num=10)        
        
     