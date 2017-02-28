#File for testing out and learning code for R Programming Coursera course in the Data Science Specialization

##Quiz Week 1
setwd("~/Dropbox/Projects/Classes/Coursera/DataScienceSpecialization/RProgramming/courseraRProgramming")
data<-read.csv("hw1_data.csv")
names(data)
data[1:2,]
dim(data)
tail(data)
data[152:153,]
data[47,]
is.na(data[,1])
missingOzone<-is.na(data[,1])
countmissing<-as.numeric(missingOzone)
countmissing
sum(countmissing)
ozone<-data[,1]
ozone
bad<-is.na(ozone)
goodozone<-ozone[!bad]
goodozone
mean(goodozone)
part<-data[data$Ozone>31 & data$Temp>90,]
part
good<-complete.cases(part)
goodpart<-part[good,]
goodpart
mean(goodpart$Solar.R)
Solar.R<-part$Solar.R
Solar.R
mean(Solar.R)
bad<-is.na(Solar.R)
goodSR<-Solar.R[!bad]
mean(goodSR)
june<-data[data$Month==6,]
june
mean(june$Temp)
may<-data[data$Month==5,]
may
##Week 1 SWIRL Assignments
install.packages("swirl")
install.packages(c("curl","httr")) ##recommended by Mentor in forum to solve swirl issues
library(swirl)
install_from_swirl("R Programming")
swirl()

##Week2##
x<-c("a", "b", "c", "d")
for(i in 1:4){
      print(x[i])
}      
for(i in seq_along(x)){
      print(x[i])
}
for(letter in x){
      print(letter)
}
for(i in 1:4) print(x[i])
x<-matrix(1:6,2,3)
for(i in seq_len(nrow(x))) {
    for(j in seq_len(ncol(x))) {
          print(x[i,j])
    }
}
count<-0
while(count < 10) {
        print(count)
        count <- count + 1
}
z<-5
while(z>=3 && z<=10) {
        print(z)
        coin <- rbinom(1, 1, 0.5)
        
        if(coin == 1) {
                z <- z+1
        } else {
                z <- z-1
        }
}
##Functions##
add2<- function(x,y) {
        x+y
}
add2(5,3)

above10 <- function(x) {
        use <- x > 10
        x[use]
}

above <- function(x, n = 10) {
      use <- x>n
      x[use]
}

columnmean <- function(y, removeNA = TRUE) {
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc) {
                  means[i] <- mean(y[,i], na.rm = removeNA)
        }
        means
}

mydata <- rnorm(100)
sd(mydata)
sd(x=mydata)
sd(x=mydata, na.rm=FALSE)
sd(na.rm=FALSE, x=mydata)

f <- function(a,b) {
      a^2
}

myplot <- function(x,y,type="l", ...) {
        plot (x,y,type=type, ...)
}

lm <- function(x) {x*x}
lm
search()

f <- function(x,y){
  x^2 +y / z
}

make.power <- function(n) {
      pow <- function(x) {
              x^n
      }
      pow
}
cube <- make.power(3)
square <- make.power(2)
cube(3)
square(3)

##Date & Time

x <- Sys.time()
x
p <- as.POSIXlt(x)
names(unclass(p))
p$sec
unclass(x)
x$sec

datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
x <- strptime(datestring, "%B %d, %Y %H:%M")
x
class(x)

x <- as.Date("2012-01-01")
y <- strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S")
x-y
x <- as.POSIXlt(x)
x-y

##SWIRL exercises
mad_libs <- function(...){
  args <- list(...)
  place <- args[["place"]]
  adjective <- args [["adjective"]]
  noun <- args [["noun"]]
  paste("News from", place, "today where", adjective, "students took to the streets in protest of the new", noun, "being installed on campus.")
}

##Quiz - Week 2
cube <- function(x,n) {
        x^3
}

x <- 1:10
if(x>5){
      x <- 0
}

f <- function(x) {
      g <- function(y) {
            y + z
      }
      z <- 4
      x + g(x)
}

x <- 5
y <- if(x<3) {
      NA
} else {
        10
}

##Week 3

x <- list(a=1:5, b=rnorm(10))
lapply(x,mean)

#anonymous functions

x <- list(a=matrix(1:4,2,2), b=matrix(1:6,3,2))
x
lapply(x, function(elt) elt[,1])

#apply

x <- matrix(rnorm(200), 20, 10)
x
apply(x, 2, mean)
apply(x,1,sum)
colMeans(x)
rowSums(x)

#mapply

noise <- function(n, mean, sd) {
        rnorm(n, mean, sd)
}
noise(5,1,2)

#split
library(datasets)
head(airquality)
s <- split(airquality, airquality$Month)
s
lapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R", "Wind")], na.rm=TRUE))
  
#debugging

printmessage <- function(x) {
        if(x >0)
                print("x is greater than zero")
        else
                print("x is less than or equal to zero")
        
        invisible(X)
}
printmessage(x=1)
printmessage(NA)

printmessage2 <- function(x) {
        if(is.na(x))
                print("x is a missing value!")
        else if(x>0)
                print("x is greater than zero")
        else if(x<0)
                print("x is less than or equal to zero")
        invisible(x)
}
x <- log(-1)
printmessage2(x)
  
