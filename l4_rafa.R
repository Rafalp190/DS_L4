install.packages(c("RCurl", "tm", "wordcloud", "quanteda", "ngram"))


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
news_txt = iconv(twittertxt, to="ASCII//TRANSLIT")

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

# lleva a minúsculas
tweets  <- tm_map(tweet_corpus, tolower)
blogs  <- tm_map(blog_corpus, tolower)
news  <- tm_map(news_corpus, tolower)

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

