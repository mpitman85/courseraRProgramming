## Figuring it out


data001 <- read.csv("001.csv")
head(data001)
good <- complete.cases(data001)
sum(good)

directory <- "~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata"
id <- 1:2
filenames <- list.files(directory)
subset <- filenames[id]
class(subset)
files <- lapply (subset, read.csv)
lapply(files, head)
subnames <- substr(subset, 1, nchar(subset)-4)
subnameslist <- as.list(subnames)
subnameslist
class(subnameslist)


filesID <- Map(cbind, files, id = subnameslist)
lapply(filesID, head)

totaldata <- do.call(rbind, filesID)
head(totaldata)
complete.cases(totaldata)
totaldata$nobs <- complete.cases(totaldata)
head(totaldata)
aggregate(nobs ~ id, totaldata, sum)

summary(files)
row.names(files) <- subset

##Testing out with similar small data frame
test <- data.frame(c("001", "001", "001", "002", "002", "002"), c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE))
names(test) <- c("id", "nobs")
test
test$id
test$nobs
completecases <- tapply(test$nobs, test$ID, sum)
length(test$nobs)
length(test$id)

completecases <- aggregate(nobs ~ id, test, FUN=sum)
completecases
class(completecases)
names(completecases) <- c("id", "nobs")
completecases                                     

##Function
complete <- function(directory, id = 1:332) {
        
        setwd(directory)
        
        filenames <- list.files(directory)
        subset    <- filenames[id]
        files     <- lapply(subset, read.csv)  
        subnames  <- substr(subset, 1, nchar(subset)-4)
        subnameslist <- as.list(subnames)
        filesID <- Map(cbind, files, id = subnameslist)
        totaldata <- do.call(rbind, filesID)
        totaldata$nobs <- complete.cases(totaldata)
        
        aggregate(nobs ~ id, totaldata, sum)
}

##Does it work?
complete("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata",
         id = c(2,4,6))

##Is it correct?
data001 <- read.csv("001.csv")
head(data001)
data001$nobs <- complete.cases(data001)
head(data001)
sum(data001$nobs)

complete("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata",
         id = 1)
