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