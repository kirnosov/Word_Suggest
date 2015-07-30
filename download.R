library(RWekajars)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(qdap)
library(NLP)
library(tm)
library(SnowballC)
library(slam)
library(RWeka)
library(rJava)
library(wordcloud)
library(stringr)
library(DT)
library(stringi)
library(googleVis)
library(ggplot2)
library(plyr)
library(stylo)

if (!file.exists("data")) {
        dir.create("data")
        fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(fileUrl, destfile = "./data/CS.zip", method = "curl")
        list.files("./data")
        dateDownloaded <- date()
        
        unzip('data//CS.zip',exdir='data/')
        list.files('data/final/')
        list.files('data/final//en_US')
}

# read the data
twits <- readLines('data/final/en_US/en_US.twitter.txt', encoding = 'UTF-8')
news <- readLines('data/final/en_US/en_US.news.txt', encoding = 'UTF-8')
blogs <- readLines('data/final/en_US/en_US.blogs.txt', encoding = 'UTF-8')

# count the data

count_twits <- str_length(twits)
count_news <- str_length(news)
count_blogs <- str_length(blogs)
max(count_twits)
max(count_news)
max(count_blogs)

# make corpus and corpus10000
TheSample <- c(twits,news,blogs)

curse_words <- rownames(read.csv("https://gist.github.com/jamiew/1112488/raw/
                                 7ca9b1669e1c24b27c66174762cb04e14cf05aa7/google_twunter_lol",
                                 sep=":"))
curse_words<-curse_words[1:(length(curse_words)-1)]

tokenize_n <- function(theCorpus, ngramCount) {
        ngram <- NGramTokenizer(theCorpus,Weka_control(min = ngramCount, max = ngramCount,
                                                       delimiters = " \\r\\n\\t.,;:\"()?!"))
        ngram <- data.frame(table(ngram))
        ngram <- ngram[order(ngram$Freq, 
                                             decreasing = TRUE),]
        colnames(ngram) <- c("String","Count")
        ngram
}

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 

get_tokens<-function(Sample){
        cleanSample <- Corpus(VectorSource(Sample))
        cleanSample <- tm_map(cleanSample,content_transformer(removePunctuation))
        cleanSample <- tm_map(cleanSample,content_transformer(removeNumbers))
        cleanSample <- tm_map(cleanSample,stripWhitespace)
        cleanSample <- tm_map(cleanSample,removeWords, stopwords("english"))
        cleanSample <- tm_map(cleanSample,content_transformer(removeURL))
        cleanSample <- tm_map(cleanSample,removeWords, as.character(curse_words))
        cleanSample <- tm_map(cleanSample,stemDocument,language = ("english"))
        cleanSample <- tm_map(cleanSample,stripWhitespace)
        Corpus <-data.frame(text=unlist(sapply(cleanSample,`[`, "content")), 
                                   stringsAsFactors = FALSE)
        unigram <- tokenize_n(Corpus,1)
        bigram <- tokenize_n(Corpus,2)
        trigram <- tokenize_n(Corpus,3)
        quadgram <- tokenize_n(Corpus,4)
        return(list("Unigram"=unigram,
                    "Bigram"=bigram,
                    "Trigram"=trigram,
                    "Quadgram"=quadgram))
}

unigram <- data.frame(String=factor(),Count=integer())
bigram <-  data.frame(String=factor(),Count=integer())
trigram <-  data.frame(String=factor(),Count=integer())
quadgram <-  data.frame(String=factor(),Count=integer())

NS=length(TheSample)
vec<-seq(1,NS,10000)
for(i in vec[1:20]){
        nmin=i
        nmax=min((i+9999),NS)
        tokens <- get_tokens(TheSample[nmin:nmax])
        
        unigram <- rbind(unigram,tokens$Unigram)
        unigram <- aggregate(unigram[,2],by=list(String=unigram$String),FUN=sum)
        colnames(unigram)<-c("String","Count")
        
        bigram <- rbind(bigram,tokens$Bigram)
        bigram <- aggregate(bigram[,2],by=list(String=bigram$String),FUN=sum)
        colnames(bigram)<-c("String","Count")
        
        trigram <- rbind(trigram,tokens$Trigram)
        trigram <- aggregate(trigram[,2],by=list(String=trigram$String),FUN=sum)
        colnames(trigram)<-c("String","Count")
        
        quadgram <- rbind(quadgram,tokens$Quadgram)
        quadgram <- aggregate(quadgram[,2],by=list(String=quadgram$String),FUN=sum)
        colnames(quadgram)<-c("String","Count")
        
        print(paste0(round(nmax/NS*100,2),"% done"))
}

saveRDS(unigram, file = "unigram_big.RDS")
saveRDS(bigram, file = "bigram_big.RDS")
saveRDS(trigram, file = "trigram_big.RDS")
saveRDS(quadgram, file = "quadgram_big.RDS")

