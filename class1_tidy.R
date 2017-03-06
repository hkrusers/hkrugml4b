### data

### footballers data from SportAnalytics. Basically the data for Konami

## install.packages('SportsAnalytics')
## require(SportsAnalytics)

## data(EURO4PlayerSkillsSep11)
## euro <- tbl_df(EURO4PlayerSkillsSep11)
## saveRDS(euro, "euro.RDS")

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


### "I fear not the man who has practiced 10000 kicks once, but I fear the man who has practiced one kick 10000 times." ~ Bruce Lee

## One kick: Five verbs function


## Verb2: select(): select columns

euro %>% select(Name, Position, Stamina)

## Verb 1: filter(): select rows

euro %>% filter(Stamina > 95)

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

## Sort by multiple columns

euro %>% arrange(desc(Stamina), Position) %>% 
  select(Name, Position, Stamina)

## Verb 4: mutate

euro %>% select(Name, Weight, Height)

euro %>% mutate(BMI = Weight / (Height / 100)^2) %>% 
  select(Name, Weight, Height, BMI)

## Arrange by BMI

euro %>% mutate(BMI = Weight / (Height / 100)^2) %>% 
  select(Name, Weight, Height, BMI) %>% arrange(desc(BMI))

euro %>% mutate(BMI = Weight / (Height / 100)^2) %>% 
  select(Name, Weight, Height, BMI) %>% arrange(BMI)

euro %>% mutate(BMI = Weight / (Height / 100)^2) %>% 
  filter(Stamina > 10) %>% 
  select(Name, Weight, Height, BMI, Stamina, Age, Position) %>% 
  ggplot(aes(x = Age, y = Stamina, col = Position)) + geom_point()

## Verb 5: Summarise

euro %>% summarise(mean_sta = mean(Stamina), sd_sta = sd(Stamina))

## group_by

euro %>% group_by(Position)

euro %>% group_by(Position) %>% 
  summarise(mean_sta = mean(Stamina), sd_sta = sd(Stamina))

## Remember the outlier

euro %>% filter(Stamina > 0) %>% group_by(Position) %>% 
  summarise(mean_sta = mean(Stamina), sd_sta = sd(Stamina))

## Explain the following pipelines

euro %>% filter(Position != "Goalkeeper" & Stamina > 0) %>% 
  group_by(League) %>% summarise(mean_acc = mean(ShotAccuracy)) %>% 
  arrange(mean_acc)

euro %>% filter(Position != "Goalkeeper" & Stamina > 0) %>% 
  group_by(Team) %>% summarise(mean_acc = mean(ShotAccuracy)) %>% 
  arrange(desc(mean_acc))

euro %>% filter(Stamina > 0) %>% group_by(Position) %>% 
  mutate(staminaz = (Stamina - mean(Stamina)) / sd(Stamina)) %>% 
  select(Name, Position ,staminaz) %>% arrange(desc(staminaz))


euro %>% filter(Stamina > 0) %>% group_by(Position) %>% 
  mutate(staminaz = mean(Stamina)) %>% select(staminaz)

### top_n: select the best n based on a column

euro %>% filter(Stamina > 0) %>% group_by(Position) %>% 
  mutate(staminaz = (Stamina - mean(Stamina)) / sd(Stamina)) %>% 
  select(Name, Position ,staminaz) %>%
  top_n(2, staminaz) %>% arrange(Position, desc(staminaz))

### tally(): count no of cases

euro %>% filter(Stamina > 0) %>% filter(League == "SerieA") %>% 
  group_by(Nationality) %>% tally 
