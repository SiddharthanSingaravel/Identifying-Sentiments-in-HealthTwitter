# Naive Bayes

library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)

# Parallel processing
library(doMC)
registerDoMC(cores=detectCores())

# read data
df<- read.csv("movie-pang02.csv", stringsAsFactors = FALSE)
glimpse(df)

# randomize df
set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
glimpse(df)

# Factor conversion
# Convert the 'class' variable from character to factor.
df$class <- as.factor(df$class)

# Corpus and vectorize it
corpus <- Corpus(VectorSource(df$text))
inspect(corpus[1:3])

# Data cleaning_load tidyr
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

# Documentation matrix
dtm <- DocumentTermMatrix(corpus.clean)


# Partitioning the data; use 3:1 ratio for good results and change n in df based on # of tweets
df.train <- df[1:1500,]
df.test <- df[1501:2000,]

dtm.train <- dtm[1:1500,]
dtm.test <- dtm[1501:2000,]

corpus.clean.train <- corpus.clean[1:1500]
corpus.clean.test <- corpus.clean[1501:2000]

# Feature selection
dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))

# Using the 5 most frequent words for the DTM
dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)

tm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.train.nb)

# Boolean feature multinomial Naive Bayes
# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs

trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

# Training the Naive Bayes Model
# Training the classifier 

system.time( classifier <- naiveBayes(trainNB, df.train$class, laplace = 1) )

# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=testNB) )

# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions"= pred,  "Actual" = df.test$class )

# Confusion matrix
# Prepare the confusion matrix
conf.mat <- confusionMatrix(pred, df.test$class)

conf.mat
conf.mat$byClass
conf.mat$overall


# Prediction Accuracy
conf.mat$overall['Accuracy']
