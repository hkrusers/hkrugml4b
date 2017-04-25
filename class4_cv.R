require(tidyverse)
require(e1071)

wage <- readRDS('wage.RDS')

wage <- tbl_df(wage)

summary(wage)

### how to predict the wage?

### split

set.seed(689)
training <- sample(1:3000, 2000)

trainingset <- wage[training,]
trainingset <- wage[-training,]

### No free hunch! We need to tune that!
### our aim is to have the best prediction in the test-set
### our aim is to have the best prediction in the test-set
### our aim is to have the best prediction in the test-set

## 1. use more feature -> overfit
## 2. use less feature -> underfit
## 3. use advance algorithm -> overfit/underfit

lm1 <- glm(wage~age+maritl+race+education+jobclass+health+health_ins, data= trainingset)

lm2 <- glm(wage~age+race+education+jobclass+health, data= trainingset)
svmm <- svm(wage~age+maritl+race+education+jobclass+health+health_ins, data= trainingset)

svmm2 <- svm(wage~age+maritl+race+education, data= trainingset)



rmse <- function(y, y_pred) {
    sqrt(mean((y-y_pred)^2))
}

plot(predict(lm1), trainingset$wage)

rmse(trainingset$wage, predict(lm1))
rmse(trainingset$wage, predict(lm2))
rmse(trainingset$wage, predict(svmm))
rmse(trainingset$wage, predict(svmm2))


#### We know nothing about the anticipated test-set error!
### Use some of your training set as cross-validation set

set.seed(689)
cv <- sample(1:10, nrow(trainingset), replace = TRUE)

trainingset[cv==1,] %>% nrow
trainingset[cv!=1,] %>% nrow

### use the first fold

lmcv1 <- glm(wage~age+maritl+race+education+jobclass+health+health_ins, data= trainingset[cv!=1,])

predict(lmcv1, trainingset[cv==1,])

rmse(trainingset[cv==1,]$wage, predict(lmcv1, trainingset[cv==1,]))


### use the second fold

lmcv1 <- glm(wage~age+maritl+race+education+jobclass+health+health_ins, data= trainingset[cv!=2,])

predict(lmcv1, trainingset[cv==2,])

rmse(trainingset[cv==2,]$wage, predict(lmcv1, trainingset[cv==2,]))

### do all ten fold

glmcv <- function(x, formula = wage~age+maritl+race+education+jobclass+health+health_ins) {
    lmcv1 <- glm(formula, data= trainingset[cv!=x,])
    rmse(trainingset[cv==x,]$wage, predict(lmcv1, trainingset[cv==x,]))
}

### verify
glmcv(1)
glmcv(2)

cv_errors <- map_dbl(1:10, glmcv)

mean(cv_errors)

cv_errors <- map_dbl(1:10, glmcv, formula = wage~age+race+education+jobclass+health)

mean(cv_errors)


### your turn: construct a cv_svm function

### compare the mean cv_error for the following

##1. glm(wage~age+maritl+race+education+jobclass+health+health_ins)
##2. glm(wage~age+maritl+race+education+jobclass)
##3. svm(wage~age+maritl+race+education+jobclass+health+health_ins, scale = FALSE)
##4. svm(wage~age+maritl+race+education+jobclass, scale = FALSE)
##5. svm(wage~age+maritl+race+education+jobclass+health+health_ins, SCALE = TRUE)
##6. svm(wage~age+maritl+race+education+jobclass, scale = TRUE)


titanic <- read.csv('titanic.csv', stringsAsFactors = FALSE) %>% tbl_df

titanic$age
titanic$age[is.na(titanic$age)] <- mean(titanic$age, na.rm = TRUE)

titanic %>% 
  mutate(survived = as.factor(survived), 
         sex = as.factor(sex), 
         pclass = as.factor(pclass)) -> titanic

set.seed(689)
training <- sample(1:nrow(titanic), size = 1000)

trainingset <- titanic[training,]
testset <- titanic[-training,]

### The correct way to approach this problem
## ### our aim is to have the best prediction in the test-set
## ### our aim is to have the best prediction in the test-set
## ### our aim is to have the best prediction in the test-set

### use more feature, use more advance algo


glm(survived~sex+pclass, data = trainingset, family = binomial) -> lrm1
glm(survived~sex+pclass+age, data = trainingset, family = binomial) -> lrm2

svm(survived~sex+pclass, data = trainingset, type = 'C-classification') -> svmm1
svm(survived~sex+pclass+age, data = trainingset, type = 'C-classification') -> svmm2

table(ifelse(predict(lrm1) > 0.5, 1, 0), trainingset$survived)
mean(ifelse(predict(lrm1) > 0.5, 1, 0) == trainingset$survived)

table(ifelse(predict(lrm2) > 0.5, 1, 0), trainingset$survived)
mean(ifelse(predict(lrm2) > 0.5, 1, 0) == trainingset$survived)

table(predict(svmm1), trainingset$survived)
mean(predict(svmm1) == trainingset$survived)

table(predict(svmm2), trainingset$survived)
mean(predict(svmm2) == trainingset$survived)

### No free hunch! Test yourself using cross validation!

### LOOCV

glm(survived~sex+pclass, data = trainingset[-1,], family = binomial) -> lrm1
ifelse(predict(lrm1, newdata = trainingset[1,]) > 0.5, 1, 0)

trainingset[1,]$survived


glm(survived~sex+pclass, data = trainingset[-3,], family = binomial) -> lrm1
ifelse(predict(lrm1, newdata = trainingset[3,]) > 0.5, 1, 0)
trainingset[3,]$survived

### packit

loocv <- function(x) {
    glm(survived~sex+pclass, data = trainingset[-x,], family = binomial) -> lrm1
    return(ifelse(predict(lrm1, newdata = trainingset[x,]) > 0.5, 1, 0))
}

lrm1loocv <- sapply(1:1000, loocv)


require(tm)
require(e1071)
require(class)

##install.packages('SnowballC')

spambase <- read.csv("spambase.csv", stringsAsFactors = FALSE)
saveRDS(spambase, "spambase.RDS")

set.seed(777)
training <- sample(1:5574, 3902)

trainingset <- spambase[training,]
testset <- spambase[-training,]

rawspamdtm <- DocumentTermMatrix(Corpus(VectorSource(trainingset$mail)), 
                                 control = list(stemming = TRUE, 
                                                removePunctuation = function(x) removePunctuation(x, preserve_intra_word_dashes = TRUE), 
                                                stopwords = stopwords('en')))

d <- Terms(rawspamdtm)

rawspamdtm2 <- DocumentTermMatrix(Corpus(VectorSource(testset$mail)), 
                                 control = list(stemming = TRUE, 
                                                removePunctuation = function(x) removePunctuation(x, preserve_intra_word_dashes = TRUE), 
                                                stopwords = stopwords('en'),
                                                dictionary = d))



#View(spambase)

rawspamdtm <- DocumentTermMatrix(Corpus(VectorSource(spambase$mail)), 
                                 control = list(stemming = TRUE, 
                                                removePunctuation = function(x) removePunctuation(x, preserve_intra_word_dashes = TRUE), 
                                                stopwords = stopwords('en')))

## saveRDS(rawspamdtm, "rawspamdtm.RDS")

## rawspamdtm <- readRDS('rawspamdtm.RDS')
## ### remember this?

## ### Why 3? Why 10? not 4,5,6,7,8,9,10?

## ### spamdtm <- removeSparseTerms(rawspamdtm, sparse = (nrow(rawspamdtm) - 3) / nrow(rawspamdtm))

## ### No free hunch! We need to tune that!
## ### our aim is to have the best prediction in the test-set
## ### our aim is to have the best prediction in the test-set
## ### our aim is to have the best prediction in the test-set

## set.seed(777)
## training <- sample(1:5574, 3902)
## trainingX <- spamdtm[training,]
## trainingy <- spambase$label[training]

## ### let's start with 2
## spamdtm <- removeSparseTerms(trainingX, sparse = (nrow(trainingX) - 10) / nrow(trainingX))

## svmm2 <- svm(x = as.matrix(spamdtm), y = trainingy, scale = FALSE, type = 'C-classification')

## table(predicted = predict(svmm2), actual = trainingy)

## ### We know nothing about the anticipated test-set error!

## ### Five-flow cross validation

## Terms(spamdtm)
