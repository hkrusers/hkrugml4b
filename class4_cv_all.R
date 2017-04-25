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
testset <- wage[-training,]

rmse <- function(y, y_pred) {
    sqrt(mean((y-y_pred)^2))
}

#### We know nothing about the anticipated test-set error!
### Use some of your training set as cross-validation set

set.seed(689)
cv <- sample(1:10, nrow(trainingset), replace = TRUE)

### do all ten fold

glmcv <- function(x, formula = wage~age+maritl+race+education+jobclass+health+health_ins) {
    lmcv1 <- glm(formula, data= trainingset[cv!=x,])
    rmse(trainingset[cv==x,]$wage, predict(lmcv1, trainingset[cv==x,]))
}


svmcv <- function(x, formula = wage~age+maritl+race+education+jobclass+health+health_ins, scale = TRUE) {
  lmcv1 <- svm(formula, data = trainingset[cv!=x,], scale = scale)
  rmse(trainingset[cv==x,]$wage, predict(lmcv1, trainingset[cv==x,]))
}

### compare the mean cv_error for the following

##1. glm(wage~age+maritl+race+education+jobclass+health+health_ins)
##2. glm(wage~age+maritl+race+education+jobclass)
##3. svm(wage~age+maritl+race+education+jobclass+health+health_ins, scale = FALSE)
##4. svm(wage~age+maritl+race+education+jobclass, scale = FALSE)
##5. svm(wage~age+maritl+race+education+jobclass+health+health_ins, SCALE = TRUE)
##6. svm(wage~age+maritl+race+education+jobclass, scale = TRUE)

cve1 <- mean(map_dbl(1:10, glmcv, formula = wage~age+maritl+race+education+jobclass+health+health_ins))
cve2 <- mean(map_dbl(1:10, glmcv, formula = wage~age+maritl+race+education+jobclass))
cve3 <- mean(map_dbl(1:10, svmcv, formula = wage~age+maritl+race+education+jobclass+health+health_ins, scale = FALSE))
cve4 <- mean(map_dbl(1:10, svmcv, formula = wage~age+maritl+race+education+jobclass, scale = FALSE))
cve5 <- mean(map_dbl(1:10, svmcv, formula = wage~age+maritl+race+education+jobclass+health+health_ins, scale = TRUE))
cve6 <- mean(map_dbl(1:10, svmcv, formula = wage~age+maritl+race+education+jobclass, scale = TRUE))

dotchart(c(cve1,cve2,cve3,cve4,cve5,cve6), 1:6)

### No 5 is the best

final_model <- svm(wage~age+maritl+race+education+jobclass+health+health_ins, data = trainingset, SCALE = TRUE)
rmse(testset$wage, predict(final_model, newdata = testset))
