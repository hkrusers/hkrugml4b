### Excercise 1: Interrogate the baby name datatset.

#bnames <- read.csv("https://raw.githubusercontent.com/hadley/data-baby-names/master/baby-names.csv", stringsAsFactors = FALSE)
#saveRDS(bnames, "bnames.RDS")

require(tidyverse)
require(stringr)
bnames <- readRDS('bnames.RDS')

View(bnames)
## warm up questions

## Get the following stat.

## 1. Get the baby name with the highest frequency in the year 1985

bnames %>% filter(year == 1985) %>% top_n(1, percent)

## 2. Get the male and female baby names with the highest frequency in the year 1985

bnames %>% filter(year == 1985) %>% group_by(sex) %>% top_n(1, percent)

## 3. Get the most frequent baby name for each sex by year.

bnames %>% group_by(sex, year) %>% top_n(1, percent) %>% arrange(year, sex) %>% View


## Demo. Plot the popularity of female baby with the name 'Hillary' by year

bnames %>% filter(name == 'Hillary' & sex == 'girl') %>% 
  ggplot(aes(x = year, y = percent)) + geom_line()

## 4. Plot the popularity of 'Emma' (Female) and 'Ryan' (Male) by year on the same graph


bnames %>% 
  filter((name == 'Emma' & sex == 'girl') | (name == 'Ryan' & sex == 'boy')) %>% 
  ggplot(aes(x = year, y = percent, color = name)) + geom_line()




## demo: calculate the total popularity of the first character of baby names by sex and year. Plot it as a trend graph

bnames %>% mutate(firstc = tolower(str_sub(name, 1, 1))) %>% 
  group_by(year, sex, firstc)  %>% 
  summarise(populariy = sum(percent)) %>%
  ggplot(aes(x = year, y = populariy, color = sex)) + geom_line() + facet_wrap(~ firstc, nrow = 3)

## Q1: How many unique last two characters combinations among all baby names?

bnames %>% mutate(last2 = tolower(str_sub(name, -2, -1))) %>% 
  group_by(last2)  %>% 
  summarise(popularity = sum(percent)) %>% top_n(10, popularity) -> top10last2

## Q2: Plot the popularity time trend of the most popular ten last two characters combinations by sex.

## hints: get the most popular two characters combinations first

bnames %>% mutate(last2 = tolower(str_sub(name, -2, -1))) %>% 
  filter(last2 %in% top10last2$last2) %>% group_by(year, sex, last2) %>% 
  summarise(popularity = sum(percent)) %>%
  ggplot(aes(x = year, y = popularity, color = sex)) + geom_line() + facet_wrap(~ last2, nrow = 3)

bnames %>% mutate(last2)

  
## Given
str_sub(tolower("Chainsaw"), 2, 3)
str_sub(tolower("Chainsaw"), -2, -1)

"ah" %in% c("ah", "ab", "ia")
"hi" %in% c("ah", "ab", "ia")

