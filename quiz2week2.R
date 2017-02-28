##Working on programming assignment quiz  for week 2

setwd("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming")

source("pollutantmean.R")

pollutantmean("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", "sulfate", 1:10)
pollutantmean("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", "nitrate", 70:72)
pollutantmean("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", "sulfate", 34)
pollutantmean("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", "nitrate")

setwd("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming")

source("complete.R")

cc <- complete("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

setwd("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming")

source("corr.R")

cr <- corr("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", 2000)                
n <- length(cr)                
cr <- corr("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
