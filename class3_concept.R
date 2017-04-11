## Classification problem: which your y is categorical.

## Yes / No

require(tidyverse)

### generate some rubblish data

## gp1 <- data_frame(x = rnorm(300, mean = 9, sd = 5), y = rnorm(300, mean = 3, sd = 2), gp = 'A')
## gp2 <- data_frame(x = rnorm(300, mean = 3, sd = 5), y = rnorm(300, mean = 10, sd = 3), gp = 'B')

## rbind(gp1, gp2)[sample(1:600, size = 600),] %>% saveRDS('rubbish.RDS')

rubbish <- readRDS('rubbish.RDS')

ggplot(rubbish, aes(x = x, y = y, color = gp)) + geom_point()

### Split the data
set.seed(689)
train <- sample(1:600, size = 500)

## KNN

require(class)

knn(rubbish[train, -3], rubbish[train, -3], rubbish$gp[train], k = 3)

table(knn(rubbish[train, -3], rubbish[train, -3], rubbish$gp[train], k = 3), rubbish$gp[train])

table(knn(rubbish[train, -3], rubbish[-train, -3], rubbish$gp[train], k = 3), rubbish$gp[-train])

### You may use Logistic regression

lgm <- glm(as.factor(gp)~x+y, data = rubbish[train,], family = binomial)

res <- predict(lgm, type = 'response') < 0.5
table(predicted = res, actual = rubbish[train,]$gp)

## accuracy

(res == (rubbish[train,]$gp == "A")) %>% mean

### test set

res_test <- predict(lgm, newdata = rubbish[-train,], type = 'response') < 0.5
table(predicted = res_test, actual = rubbish[-train,]$gp)

(res_test == (rubbish[-train,]$gp == "A")) %>% mean


### or recursive partitioning

## require(rpart)
## rpm <- rpart(as.factor(gp)~x+y, data = rubbish[train,])
## plot(rpm, compress = TRUE)
## text(rpm, use.n = TRUE)

## res_rpart<- round(predict(rpm, type = 'prob'), 3)
## table((res_rpart > 0.5)[,1], rubbish[train,]$gp)

## ((res_rpart > 0.5)[,1] == (rubbish[train,]$gp == "A")) %>% mean

## res_rpart_test <- predict(rpm, type = 'prob', newdata = rubbish[-train,])
## table((res_rpart_test > 0.5)[,1], rubbish[-train,]$gp)

## ((res_rpart_test> 0.5)[,1] == (rubbish[-train,]$gp == "A")) %>% mean


## OR svm (support vector machine)

require(e1071)

svmm <- svm(gp ~ x+y, data = rubbish[train,], type = 'C-classification')
#svmm <- svm(x = rubbish[train, -3], y = as.factor(rubbish$gp[train]))


table(predict(svmm), rubbish$gp[train])

table(predicted = predict(svmm, newdata = rubbish[-train, -3]), actual = rubbish$gp[-train])


### kyphosis

require(rpart)
set.seed(689)
train <- sample(1:81, size = 60)

klgm <- glm(Kyphosis ~ ., data = kyphosis[train,], family = binomial)

table(predict(klgm, type = 'response') < 0.5, kyphosis$Kyphosis[train])
table(predict(klgm, type = 'response', newdata = kyphosis[-train,]) < 0.5, kyphosis$Kyphosis[-train])

