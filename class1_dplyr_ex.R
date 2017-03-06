### Excercise 1: Interrogate the baby name datatset.

#bnames <- read.csv("https://raw.githubusercontent.com/hadley/data-baby-names/master/baby-names.csv", stringsAsFactors = FALSE)
#saveRDS(bnames, "bnames.RDS")

require(tidyverse)
require(stringr)
bnames <- readRDS('bnames.RDS')

## warm up questions

## Get the following stat.

## 1. Get the baby name with the highest frequency in the year 1985

## 2. Get the male and female baby names with the highest frequency in the year 1985

## 3. Get the most frequent baby name for each sex by year.

## Demo. Plot the popularity of female baby with the name 'Hillary' by year

bnames %>% filter(name == 'Hillary' & sex == 'girl') %>% 
  ggplot(aes(x = year, y = percent)) + geom_line()

## 4. Plot the popularity of 'Emma' (Female) and 'Ryan' (Male) by year on the same graph




## demo: calculate the total popularity of the first character of baby names by sex and year. Plot it as a trend graph

bnames %>% mutate(firstc = tolower(str_sub(name, 1, 1))) %>% 
  group_by(year, sex, firstc) %>% summarise(populariy = sum(percent)) %>%
  ggplot(aes(x = year, y = populariy, color = sex)) + geom_line() + facet_wrap(~ firstc, nrow = 3)

## Q1: How many unique last two characters combinations among all baby names?

## Q2: Plot the popularity time trend of the most popular ten last two characters combinations by sex.

## hints: get the most popular two characters combinations first



## Given
str_sub(tolower("Chainsaw"), 1, 1)
str_sub(tolower("Chainsaw"), -2, -1)

"ah" %in% c("ah", "ab", "ia")
"hi" %in% c("ah", "ab", "ia")

