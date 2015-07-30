suppressPackageStartupMessages(c(
        library(shinythemes),
        library(shiny),
        library(tm),
        library(stringr),
        library(markdown),
        library(stylo),
        library(reshape)))

prepared4Data <- readRDS(file="./data/prepared4Data.RData")
prepared3Data <- readRDS(file="./data/prepared3Data.RData")
prepared2Data <- readRDS(file="./data/prepared2Data.RData")

data_clean<-function(text){
        
        cleanText <- tolower(text)
        cleanText <- removePunctuation(cleanText)
        cleanText <- removeNumbers(cleanText)
        cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
        cleanText <- stripWhitespace(cleanText)
        
        return(cleanText)
}

input_clean <- function(text){
        
        textInput <- data_clean(text)
        textInput <- txt.to.words.ext(textInput, 
                                      language="English.all", 
                                      preserve.case = TRUE)
        
        return(textInput)
}


predict_next_word <- function(wordCount,textInput,nguess=1){
        
        if (wordCount>=3) {
                textInput <- textInput[(wordCount-2):wordCount] 
                
        }
        else if(wordCount==2) {
                textInput <- c(NA,textInput)   
        }
        
        else {
                textInput <- c(NA,NA,textInput)
        }
        wordPrediction <- as.character(prepared4Data[prepared4Data$unigram==textInput[1] & 
                                                             prepared4Data$bigram==textInput[2] & 
                                                             prepared4Data$trigram==textInput[3],][1:nguess,]$quadgram)
        
        if(is.na(wordPrediction[1])) {
                wordPrediction1 <- as.character(prepared3Data[prepared3Data$unigram==textInput[2] & 
                                                                      prepared3Data$bigram==textInput[3],][1:nguess,]$trigram)
                
                if(is.na(wordPrediction[1])) {
                        wordPrediction <- as.character(prepared2Data[prepared2Data$unigram==textInput[3],][1:nguess,]$bigram)
                }
        }
        return(wordPrediction)
}

process_message <- function(message,ng){
        if(ng==4) {
                names<-colnames(prepared4Data)
                formula = frequency ~ unigram + bigram + trigram + quadgram
                recentData <- readRDS(file="./data/recent4Data.RData")
        }
        if(ng==3) {
                names<-colnames(prepared3Data)
                formula = frequency ~ unigram + bigram + trigram
                recentData <- readRDS(file="./data/recent3Data.RData")
        }
        if(ng==2) {
                names<-colnames(prepared2Data)
                formula = frequency ~ unigram + bigram
                recentData <- readRDS(file="./data/recent2Data.RData")
        }
        textInput <- data_clean(message)
        df <- data.frame(colsplit(
                make.ngrams(input.text = txt.to.words(textInput), ngram.size = ng),
                " ",names=names[1:ng]),1)
        colnames(df)<- names
        return(aggregate(formula,data = rbind(recentData,df), sum))
}

nextWordPredictionRecent <- function(wordCount,textInput,nguess=1){
        
        recent4Data <- readRDS(file="./data/recent4Data.RData")
        recent3Data <- readRDS(file="./data/recent3Data.RData")
        recent2Data <- readRDS(file="./data/recent2Data.RData")
        
        if (wordCount>=3) {
                textInput <- textInput[(wordCount-2):wordCount] 
                
        }
        
        else if(wordCount==2) {
                textInput <- c(NA,textInput)   
        }
        
        else {
                textInput <- c(NA,NA,textInput)
        }
        
        
        ### 1 ###
        wordPrediction <- as.character(recent4Data[recent4Data$unigram==textInput[1] & 
                                                           recent4Data$bigram==textInput[2] & 
                                                           recent4Data$trigram==textInput[3],][1:nguess,]$quadgram)
        
        if(is.na(wordPrediction[1])) {
                wordPrediction1 <- as.character(recent3Data[recent3Data$unigram==textInput[2] & 
                                                                    recent3Data$bigram==textInput[3],][1:nguess,]$trigram)
                
                if(is.na(wordPrediction[1])) {
                        wordPrediction <- as.character(recent2Data[recent2Data$unigram==textInput[3],][1:nguess,]$bigram)
                }
        }
        return(wordPrediction)
}

getGoogleString <- function(searchTerms=NULL,
                            indicatorWord,
                            language="en",
                            tlwr=FALSE,
                            ...){
        
        # check for arguments
        if(is.null(searchTerms)) stop("Please enter search terms!")
        if(!any(language==c("en"))) stop("Please enter correct language (en)!")
        
        # construct google like expression
        require(RCurl)
        # Collapse search terms.
        entry <- paste(searchTerms, collapse="+")
        siteHTML <- getForm("http://www.google.com/search",
                            hl=language, lr="", q=entry,
                            btnG="Search")
        
        #   
        if(!tlwr){
                posExtractStart <- gregexpr(indicatorWord, siteHTML,fixed = TRUE)[[1]]
        }else{
                posExtractStart <- gregexpr(indicatorWord, removePunctuation(tolower(siteHTML)),
                                            fixed = TRUE)[[1]]
        }
        
        # extract string of 10 chracters length
        stringExtract <- substring(siteHTML, first=posExtractStart,
                                   last = posExtractStart + 500)
        return(stringExtract)
}

removeT <- function(x){gsub("<[^>]+>" , "", x)}
removeB <- function(x){gsub("\\[[^>]+\\]" , "", x)}
dataCleanerHard<-function(text){
        cleanText <- tolower(text)
        cleanText <- removeT(cleanText)
        cleanText <- removeB(cleanText)
        cleanText <- removePunctuation(cleanText)
        cleanText <- removeNumbers(cleanText)
        cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
        cleanText <- stripWhitespace(cleanText)
        return(cleanText)
}

nextWordPredictionGoogle <- function(textInput,Data,nguess=1){
        textInput<- txt.to.words(tolower(textInput))
        wordPrediction <- as.character(Data[Data$unigram==textInput[1] & 
                                                    Data$bigram==textInput[2] & 
                                                    Data$trigram==textInput[3] &
                                                    Data$quadgram!="quot" &
                                                    Data$quadgram!="google" &
                                                    Data$quadgram!="titlesearch",][1:nguess,]$quadgram)
        
        if(!is.na(wordPrediction)){if(nchar(wordPrediction)>14){wordPrediction=NA}}
        
        return(wordPrediction)
}

process_google <- function(message,ng=4,tlwr=FALSE){
        
        
        names<-colnames(prepared4Data)
        formula = frequency ~ unigram + bigram + trigram + quadgram
        
        words_vec <- txt.to.words(message)
        nwords <- length(words_vec)
        search_words <- paste(words_vec[nwords-2],
                              words_vec[nwords-1],
                              words_vec[nwords])
        textInput <- dataCleanerHard(getGoogleString(message,
                                                     search_words,
                                                     language="en",
                                                     tlwr=tlwr))
        df <- data.frame(colsplit(
                make.ngrams(input.text = txt.to.words(textInput), ngram.size = ng),
                " ",names=names[1:ng]),1)
        colnames(df)<- names
        data <- aggregate(formula,data = df, sum)
        nextWordPredictionGoogle(search_words,data,nguess=1)
}


