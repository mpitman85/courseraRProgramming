##Function that calculates correlation between sulfate & nitrate for monitoring stations
##with greater than the indicated threshold of complete cases in the data

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