require(tidyverse)
require(e1071)
titanic <- read.csv('titanic.csv', stringsAsFactors = FALSE) %>% tbl_df
training <- sample(1:nrow(titanic), size = 1000)

titanic$age
titanic$age[is.na(titanic$age)] <- mean(titanic$age, na.rm = TRUE)

titanic[training,] %>% mutate(survived = as.factor(survived), sex = as.factor(sex), pclass = as.factor(pclass)) %>% select(survived, sex, age, pclass) %>% svm(survived~sex+age+pclass, data = ., type = 'C-classification') -> svmm

### evaluate the performance
