##Example
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

makeVector()
cachemean(data)
cachemean
##Really don't understand how this should be used...

data <- sample(1:100, 50)
data <- as.numeric(data)
class(data)
test <- makeVector(data)
cachemean(test)
cachemean(test)
cachemean(makeVector(data))
