#install.packages(c("RCurl", "tm", "wordcloud", "quanteda", "ngram"))


library("RCurl")
library("tm")
library("wordcloud")
library("quanteda")
library("ngram")

twittertxt = readLines("data/en_US.twitter.txt",skipNul = TRUE) 
blogtxt = readLines("data/en_US.blogs.txt", skipNul = TRUE)
newscon = file("data/en_US.news.txt", 'rb')

newstxt = readLines(newscon) 

twitter_txt = iconv(twittertxt, to="ASCII//TRANSLIT")
blog_txt = iconv(blogtxt, to="ASCII//TRANSLIT")
news_txt = iconv(newstxt, to="ASCII//TRANSLIT")

tweet_corpus <- Corpus(VectorSource(twitter_txt))
blog_corpus <- Corpus(VectorSource(blog_txt))
news_corpus <- Corpus(VectorSource(news_txt))

rm(twittertxt)
rm(twitter_txt)
rm(blogtxt)
rm(blog_txt)
rm(newscon)
rm(newstxt)
rm(news_txt)

# partir el corpus
tweets_sample <- sample(tweet_corpus, round(2360148*0.03))
blogs_sample <- sample(blog_corpus, round(899288*0.03))
news_sample <- sample(news_corpus, round(1010242*0.03))


rm(tweet_corpus)
rm(news_corpus)
rm(blog_corpus)


# lleva a minúsculas
tweets  <- tm_map(tweets_sample, tolower)
blogs  <- tm_map(blogs_sample, tolower)
news  <- tm_map(news_sample, tolower)

rm(tweets_sample)
rm(news_sample)
rm(blogs_sample)

# quita espacios en blanco
tweets  <- tm_map(tweets, stripWhitespace)
blogs  <- tm_map(blogs, stripWhitespace)
news  <- tm_map(news, stripWhitespace)

# remueve la puntuación
tweets  <- tm_map(tweets, removePunctuation)
blogs  <- tm_map(blogs, removePunctuation)
news  <- tm_map(news, removePunctuation)

# remueve palabras vacias genericas
tweets <- tm_map(tweets, removeWords, c(stopwords("english")))
blogs <- tm_map(blogs, removeWords, c(stopwords("english")))
news <- tm_map(news, removeWords, c(stopwords("english")))

#remueve los numeros
tweets <- tm_map(tweets, removeNumbers)
blogs <- tm_map(blogs, removeNumbers)
news <- tm_map(news, removeNumbers)



 
# crea matriz de terminos
tweet_tdm <- TermDocumentMatrix(tweets)
tweet_terms <- findFreqTerms(tweet_tdm, lowfreq=30)

## Dataframe de frecuencias
m <- as.matrix(tweet_tdm)

v <- sort(rowSums(m),decreasing=TRUE)

tweet_freq <- data.frame(word = names(v),freq=v)

blog_tdm <- TermDocumentMatrix(blogs)
findFreqTerms(blog_tdm, lowfreq=30)
## Dataframe de frecuencias
m <- as.matrix(blog_tdm)

v <- sort(rowSums(m),decreasing=TRUE)

blog_freq <- data.frame(word = names(v),freq=v)

news_tdm <- TermDocumentMatrix(news)
findFreqTerms(news, lowfreq=30)

## Dataframe de frecuencias
m <- as.matrix(news_tdm)

v <- sort(rowSums(m),decreasing=TRUE)

tweet_freq <- data.frame(word = names(v),freq=v)


