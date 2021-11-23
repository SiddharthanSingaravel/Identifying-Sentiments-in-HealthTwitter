library(dplyr)

# read data - remove #
# data <- read.csv('../input/twitter-sentiment-analysis/Sentiment_Data.csv')
head(data)

data_1 <- data %>% 
  select(text, sentiment)
head(data_1)

round(prop.table(table(data_1$sentiment)),2)

library(tm)
# Loading required package: NLP
library(SnowballC)
corpus = VCorpus(VectorSource(data_1$text))
# a snap shot of the first text stored in the corpus
as.character(corpus[[1]])

corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
as.character(corpus[[1]])

dtm = DocumentTermMatrix(corpus)
dtm
dim(dtm)

dtm = removeSparseTerms(dtm, 0.999)
dim(dtm)

#Inspecting the the first 10 tweets and the first 15 words in the dataset
inspect(dtm[0:10, 1:15])

freq<- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
findFreqTerms(dtm, lowfreq=60) #identifying terms that appears more than 60times

library(wordcloud)
positive <- subset(data_1,sentiment=="Positive")
head(positive)
wordcloud(positive$text, max.words = 100, scale = c(3,0.5))

negative <- subset(data_1,sentiment=="Negative")
head(negative)
wordcloud(negative$text, max.words = 100, scale = c(3,0.5))

neutral <- subset(data_1,sentiment=="Neutral")
head(neutral)
wordcloud(neutral$text, max.words = 100, scale = c(3,0.5))

library("wordcloud")

# Loading required package: RColorBrewer
library(RColorBrewer)
set.seed(1234)
wordcloud(words = wf$word, freq = wf$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
datasetNB <- apply(dtm, 2, convert_count)

dataset = as.data.frame(as.matrix(datasetNB))

dataset$Class = data_1$sentiment
str(dataset$Class)

head(dataset)
dim(dataset)

# Splitting data
set.seed(222)
split = sample(2,nrow(dataset),prob = c(0.75,0.25),replace = TRUE)
train_set = dataset[split == 1,]
test_set = dataset[split == 2,] 

prop.table(table(train_set$Class))
prop.table(table(test_set$Class))

# Random forest
library(randomForest)
rf_classifier = randomForest(x = train_set[-1210],
                             y = train_set$Class,
                             ntree = 300)

rf_classifier

# Model evaluation: RF
# Predicting the Test set results
rf_pred = predict(rf_classifier, newdata = test_set[-1210])

# Making the Confusion Matrix
library(caret)

confusionMatrix(table(rf_pred,test_set$Class))