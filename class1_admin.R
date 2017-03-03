## Basic of R to get you started

## installation of external library

install.packages('tidyverse')
install.packages('rio')

## load that package

require(tidyverse)
require(rio)

## data.frame and vector

iris
iris$Sepal.Length

## a data.frame is a collection of vectors

no <- c(4,3,2,1)
male <- c(TRUE, TRUE, FALSE, FALSE)
trainer <- c("Chainsaw", "Chris", "Jane", "Lucia")

trainer[1]

## how to get "Lucia"

trainers <- data.frame(no, male, trainer)

trainers$male

trainers[1,]

trainers[,3]

trainers[4,3] # why Lucia?

## The concept of a function

## function: with input (or arguments) and output (or return value). The body of a function describes the relationship between input and output.

mean
?mean

mean(iris$Sepal.Length)

sd(iris$Sepal.Length)

## define your own function

centering <- function(x) {
    meanx <- mean(x)
    sdx <- sd(x)
    return((x-meanx)/sdx)
}

centering(iris$Sepal.Length)
meanx

ls()

## reading data

require(rio)
titanic <- import("titanic.csv")

nrow(titanic)
ncol(titanic)
colnames(titanic)
head(titanic)

table(titanic$survived)
table(titanic$pclass)

mean(titanic$age)
titanic$age

?mean
