#install.packages(c("RCurl", "tm", "wordcloud", "quanteda", "ngram"))

library("dplyr")
library("RCurl")
library("tm")
library("wordcloud")
library("quanteda")
library("ngram")

set.seed(123)

twittertxt = readLines("data/en_US.twitter.txt",skipNul = TRUE) 
blogtxt = readLines("data/en_US.blogs.txt", skipNul = TRUE)
newscon = file("data/en_US.news.txt", 'rb')

newstxt = readLines(newscon) 

twitter_txt = iconv(twittertxt, to="ASCII//TRANSLIT")
blog_txt = iconv(blogtxt, to="ASCII//TRANSLIT")
news_txt = iconv(newstxt, to="ASCII//TRANSLIT")


# partir los textos
tweets_sample <- sample(twitter_txt, round(2360148*0.005))
blogs_sample <- sample(blog_txt, round(899288*0.005))
news_sample <- sample(news_txt, round(1010242*0.005))


tweet_corpus <- Corpus(VectorSource(tweets_sample))
blog_corpus <- Corpus(VectorSource(blogs_sample))
news_corpus <- Corpus(VectorSource(news_sample))

rm(twittertxt)
rm(twitter_txt)
rm(blogtxt)
rm(blog_txt)
rm(newscon)
rm(newstxt)
rm(news_txt)

# partir el corpus
#tweets_sample <- sample(tweet_corpus, round(2360148*0.0005))
#blogs_sample <- sample(blog_corpus, round(899288*0.0005))
#news_sample <- sample(news_corpus, round(1010242*0.0005))


#rm(tweet_corpus)
#rm(news_corpus)
#rm(blog_corpus)


# lleva a minúsculas
tweets  <- tm_map(tweet_corpus, tolower)
blogs  <- tm_map(blog_corpus, tolower)
news  <- tm_map(news_corpus, tolower)

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
#findFreqTerms(tweet_tdm, lowfreq=30)

## Dataframe de frecuencias
m <- as.matrix(tweet_tdm)

v <- sort(rowSums(m),decreasing=TRUE)

tweet_freq <- data.frame(word = names(v),freq=v)

blog_tdm <- TermDocumentMatrix(blogs)
#findFreqTerms(blog_tdm, lowfreq=30)
## Dataframe de frecuencias
m <- as.matrix(blog_tdm)

v <- sort(rowSums(m),decreasing=TRUE)

blog_freq <- data.frame(word = names(v),freq=v)

news_tdm <- TermDocumentMatrix(news)
#findFreqTerms(news, lowfreq=30)

## Dataframe de frecuencias
m <- as.matrix(news_tdm)

v <- sort(rowSums(m),decreasing=TRUE)

news_freq <- data.frame(word = names(v),freq=v)

#Funciones para hacer bi y trigramas

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))


## Bigramas

#Bigrama de tweets
tweet_bigram <-  TermDocumentMatrix(tweet_corpus,
                                control = list(tokenize = BigramTokenizer))

m <- as.matrix(tweet_bigram)
v <- sort(rowSums(m),decreasing=TRUE)

bi_tweet_freq <- data.frame(word = names(v),freq=v)

#Bigrama de blogs
blog_bigram <-  TermDocumentMatrix(blog_corpus,
                                    control = list(tokenize = BigramTokenizer))

m <- as.matrix(blog_bigram)
v <- sort(rowSums(m),decreasing=TRUE)

bi_blog_freq <- data.frame(word = names(v),freq=v)

#Bigrama de news
news_bigram <-  TermDocumentMatrix(news_corpus,
                                   control = list(tokenize = BigramTokenizer))

m <- as.matrix(news_bigram)
v <- sort(rowSums(m),decreasing=TRUE)

bi_news_freq <- data.frame(word = names(v),freq=v)


## Trigramas

#Trigrama tweets
twitter_trigram <- tm::TermDocumentMatrix(tweet_corpus, 
                                             control = list(tokenize = TrigramTokenizer))
m <- as.matrix(twitter_trigram)
v <- sort(rowSums(m),decreasing=TRUE)

tri_twitter_freq <- data.frame(word = names(v),freq=v)

#Trigrama blogs
blog_trigram <- tm::TermDocumentMatrix(blog_corpus, 
                                          control = list(tokenize = TrigramTokenizer))
m <- as.matrix(blog_trigram)
v <- sort(rowSums(m),decreasing=TRUE)

tri_blog_freq <- data.frame(word = names(v),freq=v)

#Trigrama news
news_trigram <- tm::TermDocumentMatrix(news_corpus, 
                                       control = list(tokenize = TrigramTokenizer))
m <- as.matrix(news_trigram)
v <- sort(rowSums(m),decreasing=TRUE)

tri_news_freq <- data.frame(word = names(v),freq=v)


# dataframes de frecuencias de ngramas
tweet_ngram_freq <- rbind(tweet_freq, rbind(bi_tweet_freq, tri_twitter_freq)) %>% 
  arrange(desc(freq))

blog_ngram_freq <- rbind(blog_freq, rbind(bi_blog_freq, tri_blog_freq)) %>% 
  arrange(desc(freq))

news_ngram_freq <- rbind(news_freq, rbind(bi_news_freq, tri_news_freq)) %>% 
  arrange(desc(freq))

rm(list = c('bi_blog_freq', 'bi_news_freq', 'bi_tweet_freq', 'blog_bigram', 'blog_corpus'
            , 'blog_tdm', 'blog_trigram', 'blogs', 'm', 'news', 'news_bigram', 'news_trigram', 
            'news_corpus', 'tri_blog_freq', 'tri_news_freq', 'tri_twitter_freq', 'blog_freq',
            'news_freq', 'news_tdm', 'tweet_bigram', 'tweet_corpus', 'tweet_freq', 'tweet_tdm',
            'tweets', 'twitter_trigram'
            ))

# Funcion de KNP
