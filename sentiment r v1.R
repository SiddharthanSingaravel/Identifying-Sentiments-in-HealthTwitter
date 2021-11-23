# Ib-built NLP Corpus
library(tm)

# Comment if in cloud
# setwd('C:/Users/ZoomRx/Desktop/ASCO')
# getwd()

# data <- tweets_data_r
data <- read.csv(file.choose(), header = T)
View(data)
str(data)
corpus <- iconv(data$tweet, to = "utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Cleaning text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
# stopwords() - removes 174 words

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(corpus[1:5])

#remove ASCO as it makes the process redundant
# Remove whitespaces
cleanset <- tm_map(cleanset, removeWords, c('asco','asco2020'))
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(corpus[1:5])

# TDM - term documentation matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# bar plot
w <- rowSums(tdm)
w <- subset(w, w>= 25)
barplot(w, las = 2, col = rainbow(50))

# word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
# repeatability set.seed = 200
set.seed(200)
wordcloud(words = names(w), freq = w, max.words = 150, random.order = FALSE, min.freq = 5, colors = brewer.pal(8, 'Dark2'), scale = c(7,0.3), rot.pet = 0.3)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','frequency')
wordcloud2(w,
           size = 0.8,
           shape = 'circle',
           rotateRatio = 0.3,
           minSize = 1)

# sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# read file
data <- read.csv(file.choose(), header = T)
tweets <- iconv(data$tweet, to = "utf-8")

# senti scores
s <- get_nrc_sentiment(tweets)
# head(s)

