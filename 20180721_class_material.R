# 20180721 Machine Learning for Beginners 

###########################################
# Agenda
#  0. Make sure you have installed R software and packages, as well as class material
#  1. Intro slides  (https://docs.google.com/presentation/d/1zO2_OZwlvWXMoKu4EsQ__u0fyFkG4BPdwbfdN0c77T4/edit?usp=sharing)
#  2. Recap: tidyverse
#  3. Regression
#  4. Classification
#  5. What's next?
#  6. Feedback  (https://docs.google.com/forms/d/e/1FAIpQLSdwQ8vFW7uECx5FplD7zCiIveyuRz_QD8MtEwe4JjCyy7ozyQ/viewform?usp=sf_link)
###########################################


# Required packages
library(tidyverse)
library(modelr)
library(lubridate)
library(glmnet)
library(plotly)
library(e1071)
library(rpart)
library(class)

# install.packages(c("tidyverse", "modelr", "lubridate", "glmnet", "plotly", "e1071", "rpart", "class"))

######################
## 2. Tidyverse Recap
######################
# asking for help
?lm

# read data
euro <- readRDS('euro.RDS')

require(tidyverse)

### Plotting

## Grammar of graphics

head(euro)
View(euro)

ggplot(euro)

## aesthetic: what are the meanings of x, y and color

ggplot(euro, aes(x = Age, y = Stamina))

## Geometry: visual representations of observations

ggplot(euro, aes(x = Age, y = Stamina)) + geom_point()

ggplot(euro, aes(x = Age, y = Stamina, col = Position)) + geom_point()

head(euro)

euro %>% head %>% View

View(head(euro))


### "I fear not the man who has practiced 10000 kicks once, but I fear the man who has practiced one kick 10000 times." ~ Bruce Lee

## One kick: Five verbs function


## Verb 1: select(): select columns

euro %>% select(Name, League)

## Verb 2: filter(): select rows

euro %>% filter(Stamina < 10) %>% View

## Compositity: Write programs that do one thing and do it well. Write programs to work together.

euro %>% filter(Stamina > 95) %>% 
  select(Name, Position, Stamina)

## Verb 3: arrange

## Ascending order

euro %>% arrange(Stamina) %>% 
  select(Name, Position, Stamina)

## Dscending order

euro %>% arrange(desc(Stamina)) %>% 
  select(Name, Position, Stamina)

## Verb 4: mutate

euro %>% mutate(HeightM = Height / 100) %>% View

euro %>% mutate(BMI = Weight / (Height / 100)^2) %>% 
  select(Name, Weight, Height, BMI)

## Verb 5: Summarise

euro %>% summarise(mean_sta = mean(Stamina), sd_sta = sd(Stamina))

## group_by

euro %>% group_by(Position)

euro %>% group_by(Position) %>% 
  summarise(mean_sta = mean(Stamina), median_sta = median(Stamina))


####################################################
# 3. Regression
#####################
## 1st Lab
#####################

# Data that comes with tidyverse
data(sim1)

# Lets look at it
View(sim1)

# Remember how to plot from last session?
ggplot(sim1, aes(x, y)) + 
  geom_point()

# Lets fit it
# R formula
# y = c + m * x
# y ~ 1 + x

mod <- lm(y~1+x, sim1)
mod <- lm(y~x, sim1) # same as above line

mod
# y = 4.221 + 2.052 x

# plot it
grid <- data.frame(x=-10:10)
grid$pred <- predict(mod, grid)

grid

#grid <- sim1 %>% 
#  data_grid(x) %>%
#  add_predictions(mod)

grid %>% ggplot(aes(x=x,y=pred)) + geom_line()

ggplot(sim1, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)


# BONUS: pack this as a function
plot_prediction <- function(mod, data){
  grid <- data.frame(x=seq(from=min(data$x), to=max(data$x), length.out=100))
  grid$pred <- predict(mod, grid)

  ggplot(data, aes(x)) +
    geom_point(aes(y = y)) +
    geom_line(aes(y = pred), data = grid, colour = "red", size = 1) 
}


###################
# 2nd Lab session
###################

# Practice a bit on transformation
# Load the data
library(readr)
my_data <- read_rds("./class2/my_data.rds")

# Look at what's inside
my_data %>% head()
my_data %>% ggplot(aes(x,y)) + geom_point()



# Try x^2 first
mod1 <- lm(y~x+I(x^2), my_data)
plot_prediction(mod1, my_data)

# Fitted - fitted(mod1)
# Residual - resid(mod1)
plot(fitted(mod1), resid(mod1))


# Secondary school maths
# y = c + a x^b
# log(y) = log( a * x^b)
#        = log(a) + b * log(x)
# log(y) = log(a) + b * log(x)
my_log_data <- my_data %>% mutate(y=log(y), x=log(x))
mod3 <- lm(y~x, my_log_data)
coef(mod3)
plot_prediction(mod3, my_log_data)


#######################
# Categorical Variable
data(sim2)

View(sim2)

# look at what that x is
str(sim2)

ggplot(sim2) + 
  geom_point(aes(x, y))

# Multiple indicator function
mod2 <- lm(y ~ x, data = sim2)
coef(mod2)

# Review the model matrix
model.matrix(y~x, data=sim2) %>% View()

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)




###########################################
# Off-sample evaluation
###########################################

# first simulate some simple data 
n <- 100 # sample size
hk_r_user_data <- tibble(x = runif(n),
                         y = 10 *x + rnorm(n)
)

hk_r_user_data %>% View()
# plot it
hk_r_user_data %>% ggplot(aes(x=x, y=y)) + geom_point()

data_with_random_bits <- hk_r_user_data %>%
  mutate(z1=runif(nrow(hk_r_user_data)),
         z2=runif(nrow(hk_r_user_data)),
         z3=runif(nrow(hk_r_user_data)),
         z4=runif(nrow(hk_r_user_data)),
         z5=runif(nrow(hk_r_user_data)),
         z6=runif(nrow(hk_r_user_data)),
         z7=runif(nrow(hk_r_user_data)),
         z8=runif(nrow(hk_r_user_data)),
         z9=runif(nrow(hk_r_user_data)),
         z10=runif(nrow(hk_r_user_data))
  )
data_with_random_bits %>% View()

mod <- lm(y~x , data=data_with_random_bits[1:60,])
mod_overfit <- lm(y~. , data=data_with_random_bits[1:60,])


mean(resid(mod)^2)
mean(resid(mod_overfit)^2) # even lower!!!


# Using data not within training set to evaluate gives the correct answer!
mean( (data_with_random_bits[61:n, "y"]-predict(mod, data_with_random_bits[61:n,]))^2)
mean( (data_with_random_bits[61:n, "y"]-predict(mod_overfit, data_with_random_bits[61:n,]))^2)



###############################
# Exercise - Diamond dataset 
###############################
# http://r4ds.had.co.nz/model-building.html#diamond-prices

# Diamond price dataset from ggplot package
data(diamonds)
diamonds %>% ggplot(aes(x=carat, y=price)) + 
  geom_hex(bins = 50)


# Demo with interactive plotly
library(plotly)
diamonds %>% ggplot(aes(x=carat, y=price)) + 
  geom_hex(bins = 50) -> my_plot
ggplotly(my_plot)


# Secondary school maths
# y = a x^b
# log(y) = log(a) + b * log(x)
diamonds2 <- diamonds %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

diamonds2 %>% ggplot(aes(lcarat, lprice)) + geom_hex(bins=50) -> my_plot2
ggplotly(my_plot2)



## Exercise
# Lets create a model to predict diamond price
train <- sample(1:nrow(diamonds2),30000)
diamond_mod <- lm(lprice~lcarat,data=diamonds2[train,])
diamond_mod

predict(diamond_mod, data=diamonds2[-train,]) # out of sample error



########################################################
## Classification
## Classification problem: which your y is categorical.
##
## Yes / No
########################################################

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

require(rpart)
rpm <- rpart(as.factor(gp)~x+y, data = rubbish[train,])
plot(rpm, compress = TRUE)
text(rpm, use.n = TRUE)

res_rpart<- round(predict(rpm, type = 'prob'), 3)
table((res_rpart > 0.5)[,1], rubbish[train,]$gp)

## ((res_rpart > 0.5)[,1] == (rubbish[train,]$gp == "A")) %>% mean

res_rpart_test <- predict(rpm, type = 'prob', newdata = rubbish[-train,])
table((res_rpart_test > 0.5)[,1], rubbish[-train,]$gp)

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




########################################
## Exercise: Titanic data
########################################
require(tidyverse)
require(rpart)
titanic <- read.csv('titanic.csv', stringsAsFactors = FALSE) %>% tbl_df
training <- sample(1:nrow(titanic), size = 1000)

titanic$age
titanic$age[is.na(titanic$age)] <- mean(titanic$age, na.rm = TRUE)

cleaned_titanic <- titanic[training,] %>% mutate(survived = as.factor(survived), sex = as.factor(sex), pclass = as.factor(pclass)) 



tree_model <- rpart(survived~sex+age+pclass, data=cleaned_titanic[training,])

### Evaluate the performance
table(cleaned_titanic$survived[-training], 
      (predict(tree_model, cleaned_titanic[-training, ])[,1]>0.5))


## Can we improve?






