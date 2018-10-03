#install.packages(c("RCurl", "tm", "wordcloud", "quanteda", "ngram", "tidyr", "dplyr"))

library("tidyr")
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
tweets_sample <- sample(twitter_txt, round(2360148*0.001))
blogs_sample <- sample(blog_txt, round(899288*0.001))
news_sample <- sample(news_txt, round(1010242*0.001))


all_sample <- c(tweets_sample, blogs_sample, news_sample)

all_sample <- VCorpus(VectorSource(all_sample))

rm(twittertxt)
rm(twitter_txt)
rm(blogtxt)
rm(blog_txt)
rm(newscon)
rm(newstxt)
rm(news_txt)
rm(tweets_sample)
rm(blogs_sample)
rm(news_sample)

# lleva a minúsculas
all_sample <- tm_map(all_sample, content_transformer(tolower))

# quita espacios en blanco
all_sample <- tm_map(all_sample, content_transformer(stripWhitespace))

# remueve la puntuación
all_sample <- tm_map(all_sample, content_transformer(removePunctuation))

# remueve palabras vacias genericas
all_sample <- tm_map(all_sample, content_transformer(removeWords), c(stopwords("english")))

#remueve los numeros
all_sample <- tm_map(all_sample, content_transformer(removeNumbers))


## Unigramas

# crea matriz de terminos
tdm <- TermDocumentMatrix(all_sample)
#findFreqTerms(tweet_tdm, lowfreq=30)

## Dataframe de frecuencias
m <- as.matrix(tdm)

v <- sort(rowSums(m),decreasing=TRUE)

uni_freq <- data.frame(word = names(v),freq=v) %>% 
  mutate(prob= (freq/sum(freq)))


#Funciones para hacer bi y trigramas

BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=2, max=2))}
ThreegramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=3, max=3))}


## Bigramas

bigram <- TermDocumentMatrix(all_sample, control = list(tokenize = BigramTokenizer))

m <- as.matrix(bigram)
v <- sort(rowSums(m),decreasing=TRUE)

bigram_freq <- data.frame(word = names(v),freq=v) %>% 
  mutate(prob= (freq/sum(freq))) %>% 
  separate(word, c("unigram", "predicted"), " ")

## Trigramas

trigram <- TermDocumentMatrix(all_sample, control = list(tokenize = ThreegramTokenizer))

m <- as.matrix(trigram)
v <- sort(rowSums(m),decreasing=TRUE)

trigram_freq <- data.frame(word = names(v),freq=v) %>% 
  mutate(prob = (freq/sum(freq))) %>% 
  separate(word, c("unigram1","unigram2", "predicted"), " ") %>% 
  mutate(bigram = paste0(unigram1, " ", unigram2)) %>% 
  select(bigram, everything()) %>% 
  select(-c(unigram1, unigram2))

rm(list=c('all_sample', 'bigram', 'm', 'tdm', 'trigram'))

