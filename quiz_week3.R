##Work for quiz week 3  

##question 1

library(datasets)
data(iris)
?iris
iris
names(iris)
tapply(iris$Sepal.Length, iris$Species, mean)

##question 2

sapply(iris, mean)
lapply(iris, mean)
colMeans(iris)
apply(iris[,1:4], 2, mean)
apply(iris,1,mean)
apply(iris, 2, mean)
apply(iris[,1:4],1,mean)

##question 3
library(datasets)
data(mtcars)
names(mtcars)
tapply(mtcars$mpg, mtcars$cyl, mean)
apply(mtcars,2,mean)
mean(mtcars$mpg, mtcars$cyl)
sapply(mtcars, cyl, mean)
split(mtcars, mtcars$cyl)
lapply(mtcars, mean)
with(mtcars, tapply(mpg, cyl, mean))
split(mtcars$mpg, mtcars$cyl)
sapply(split(mtcars$mpg, mtcars$cyl), mean)

##question 4
avghp <- tapply(mtcars$hp, mtcars$cyl, mean)
avghp
avghp[1]
avghp[3]
answer <- avghp[3] - avghp[1]
answer
round(answer)

##question 5
debug(ls)
ls()
Q
undebug(ls)
