##Figuring things out.
?cor
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
 
min <- 500 
screen <- complete("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata")
screen$nobs
class(screen$nobs) 

screen$min <- ifelse(screen$nobs > min, TRUE, FALSE) 
head(screen)
screenlist <- screen[screen$min == TRUE, ]
class(screenlist)
head(screenlist)
tail(screenlist) 
length(screenlist)
screenlist 
screenlist$id
class(screenlist$id)
subset <- as.character(screenlist$id)
subset
length(subset)
if(length(subset)==0) {
        numeric(length=0)}
filelist <- paste(subset, ".csv", sep = "")
filelist
data <- lapply(filelist, read.csv)

data001 <- read.csv("001.csv")
data001
cor(data001[,2], data001[,3], use = "complete.obs")

lapply(data, head) 
answer <- sapply(data, function(data)cor(data[,2], data[,3], use="complete.obs"))  
summary(answer)
##Function
setwd("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming")
source("complete.R")
getwd()
corr <- function(directory, threshold = 0) {
        
        setwd(directory)
        
        info <- complete(directory)
        info$min <- ifelse(info$nobs > threshold, TRUE, FALSE)
        screenlist <- info[info$min == TRUE, ]
        subset <- as.character(screenlist$id)
        if(length(subset) == 0) { 
                numeric(length=0)
        } else {
        filelist <- paste(subset, ".csv", sep = "")
        data <- lapply(filelist, read.csv)
        
        sapply(data, function(data)cor(data[,2], data[,3], use="complete.obs"))
        }
}

##Does it work?

corr("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming/specdata",
     threshold = 5000)
