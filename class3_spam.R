require(tm)
require(e1071)
require(class)

##install.packages('SnowballC')

spambase <- read.csv("spambase.csv", stringsAsFactors = FALSE)

rawspamdtm <- DocumentTermMatrix(Corpus(VectorSource(spambase$mail)), control = list(stemming = TRUE, removePunctuation = function(x) removePunctuation(x, preserve_intra_word_dashes = TRUE), stopwords = stopwords('en')))

spamdtm <- removeSparseTerms(rawspamdtm, sparse = (nrow(rawspamdtm) - 3) / nrow(rawspamdtm))

set.seed(777)
training <- sample(1:5574, 3902)
trainingX <- as.matrix(spamdtm[training,])
trainingy <- as.factor(spambase$label[training])

svmm <- svm(trainingX, trainingy, scale = FALSE)
table(predict(svmm), trainingy)

spamdtm <- removeSparseTerms(spamdtm, sparse = (nrow(spamdtm) - 10) / nrow(spamdtm))
trainingX <- spamdtm[training,]
trainingy <- spambase$label[training]

svmmodel <- svm(x = as.matrix(trainingX), y = as.factor(trainingy))

spamdtm <- removeSparseTerms(rawspamdtm, sparse = (nrow(rawspamdtm) - 10) / nrow(rawspamdtm))

set.seed(777)
training <- sample(1:5574, 3902)
trainingX <- as.matrix(spamdtm[training,])
trainingy <- as.factor(spambase$label[training])
svmmodel <- svm(x = as.matrix(trainingX), y = as.factor(trainingy))
table(predict(svmmodel), trainingy)

testX <- spamdtm[-training,]
testy <- spambase$label[-training]
table(predict(svmmodel, as.matrix(testX)), testy)

