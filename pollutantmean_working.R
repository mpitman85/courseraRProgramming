##Figuring it out
setwd("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/Rprogramming/courseraRProgramming")
       
getwd()
data001 <- read.csv("001.csv")
data001
head(data001)
class(data001)
data002 <- read.csv("002.csv")
totaldata <- rbind(data001, data002)
totaldata
dim(data001)
dim(totaldata)
mean(totaldata$sulfate, na.rm=TRUE)
read.csv("1.csv")

directory <- "specdata"
pollutant <- "sulfate"
id <- 1:2
filenames <- list.files(directory)
filenames
subset <- filenames[id]
subset
files <- lapply(subset, read.csv, )
files
summary(files)
class(files)
totaldata <- do.call(rbind, files)
names(totaldata)
mean(totaldata$"pollutant", na.rm=TRUE)
unclass(pollutant)

pollutantmean <- function(directory, pollutant, id = 1:332) {
        setwd(directory)
                     filenames <- list.files(directory)
                     subset    <- filenames[id]
                     files     <- lapply(subset, read.csv)
                     totaldata <- do.call(rbind, files)
                     sulfate <- totaldata[,2]
                     nitrate <- totaldata[,3]
                     if (pollutant == "sulfate") { p <- sulfate
                     } else { p <- nitrate 
                     }
                     
                     mean(p, na.rm=TRUE)
}
args(pollutantmean)

pollutantmean(directory = "~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", 
              pollutant = "nitrate", id = 1:2)

##Is it right?
data001 <- read.csv("001.csv")
mean(data001$sulfate, na.rm=TRUE)
pollutantmean("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", 
              pollutant = "sulfate")
mean(data001$nitrate, na.rm=TRUE)
pollutantmean("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", 
              pollutant = "nitrate", id = 1)

