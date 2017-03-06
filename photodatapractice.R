setwd("/Users/meganpitman/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming")

data <- read.csv("KAFBexport151001to161001duplicaterows.csv")

install.packages("dplyr")
library(dplyr)

date <- data$DateTaken ##Set DateTaken column to date vector
date <- as.character(date) ##Change date vector class to character
date <- strptime(date, "%m/%d/%y %H:%M") ##Change date vector class to POSIXlt time format
date <- as.POSIXct(date) ##Change date vector class to POSIXct time format
class(date) ##Check that class is correct
data$DateTaken <- date ##replace DateTaken column (formatted as levels) with date format
class(data$DateTaken) ##Check that column still has POSIXct format
data <- mutate(data, year=as.POSIXlt(data$DateTaken)$year +1900) ##Create year column
data <- mutate(data, month=as.POSIXlt(data$DateTaken)$mon+1) ##Create month column
data$count <- 1 ##Create count column
data <- mutate(data, GroupTotal=Group_Male + Group_Female + Group_Juvenile + 
                       Group_Indeterminate) ##Create GroupTotal column
head(data) ##Look at data
tail(data) ##Look at data
with(data, tapply(GroupTotal, ##Create table of species totals by month
            list(Group_Species_LatinName, month), sum)) 





