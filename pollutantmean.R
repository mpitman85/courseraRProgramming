## Function that returns mean for specified pollutant from specdata

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


