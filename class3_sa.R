require(tidyverse)

sa20k <- read.csv("SA200K.csv", stringsAsFactors = FALSE) %>% tbl_df

sa20k %>% select(Sentiment, SentimentText) %>% head %>% data.frame

### selection of 70% as training set

train <- sample(1:nrow(sa20k), floor(nrow(sa20k) * 0.7))

### train a SVM model to classify the sentiment
