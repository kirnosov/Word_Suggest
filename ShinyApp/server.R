suppressPackageStartupMessages(c(
        library(shinythemes),
        library(shiny),
        library(tm),
        library(stringr),
        library(markdown),
        library(stylo)))

source("helper.R")
prepared4Data <- readRDS(file="./data/prepared4Data.RData")
prepared3Data <- readRDS(file="./data/prepared3Data.RData")
prepared2Data <- readRDS(file="./data/prepared2Data.RData")

recent4Data <- readRDS(file="./data/recent4Data.RData")
recent3Data <- readRDS(file="./data/recent3Data.RData")
recent2Data <- readRDS(file="./data/recent2Data.RData")

shinyServer(function(input, output, session) {
        
        wordPrediction <- reactive({
                text <- input$text
                textInput <- input_clean(text)
                wordCount <- length(textInput)
                wordPrediction <- predict_next_word(wordCount,textInput,nguess=3)
        })
        wordPredictionRecent <- reactive({
                text <- input$text
                textInput <- input_clean(text)
                wordCount <- length(textInput)
                wordPredictionRecent <- nextWordPredictionRecent(wordCount,textInput,nguess=1)
        })
        
        wordPredictionGoogle <- reactive({
                words_vec_1 <- txt.to.words(input$text)
                words_vec_2 <- txt.to.words(input$text,splitting.rule = "[[:space:]]+")
                nvec_1<-length(words_vec_1)
                nvec_2<-length(words_vec_2)
                if_switch=FALSE
                if(nvec_1 > 3 & nvec_2 > 3){
                        if_switch <- identical(words_vec_1[(nvec_1-2):nvec_1],
                                               words_vec_2[(nvec_2-2):nvec_2])
                }
                if(if_switch){
                        text <- input$text
                        wordPredictionGoogle <- process_google(text,tlwr=FALSE)
                }else{
                        text <- removePunctuation(tolower(input$text))
                        wordPredictionGoogle <- process_google(text,tlwr=TRUE)
                }
                wordPredictionGoogle
        })
        
        output$word1 <- renderUI({
                actionButton("insert1", wordPrediction()[1])
        })
        output$word2 <- renderUI({
                actionButton("insert2", wordPrediction()[2])
        })
        output$word3 <- renderUI({
                actionButton("insert3", wordPrediction()[3])
        })
        
        output$wordR <- renderUI({
                actionButton("insertR", wordPredictionRecent())
        })
        
        output$wordG <- renderUI({
                actionButton(inputId="insertG",
                             label=wordPredictionGoogle()[1]
                )
        })
        
        
        observeEvent(input$insert1, {
                updateTextInput(session, "text",
                                value = paste0(input$text, 
                                               ifelse(!is.na(wordPrediction()[1]),
                                                      paste0(" ",wordPrediction()[1]),
                                                      ""))
                )
        })
        observeEvent(input$insert2, {
                updateTextInput(session, "text",
                                value = paste0(input$text, 
                                               ifelse(!is.na(wordPrediction()[2]),
                                                      paste0(" ",wordPrediction()[2]),
                                                      ""))
                )
        })
        observeEvent(input$insert3, {
                updateTextInput(session, "text",
                                value = paste0(input$text, 
                                               ifelse(!is.na(wordPrediction()[3]),
                                                      paste0(" ",wordPrediction()[3]),
                                                      ""))
                )
        })
        observeEvent(input$insertR, {
                updateTextInput(session, "text",
                                value = paste0(input$text, 
                                               ifelse(!is.na(wordPredictionRecent()),
                                                      paste0(" ",wordPredictionRecent()),
                                                      ""))
                )
        })
        observeEvent(input$insertG, {
                updateTextInput(session, "text",
                                value = paste0(input$text, 
                                               ifelse(!is.na(wordPredictionGoogle()[1]),
                                                      paste0(" ",wordPredictionGoogle()[1]),
                                                      ""))
                )
        })
        
        observeEvent(input$send, {
                recent4Data<-process_message(input$text,ng=4)
                saveRDS(recent4Data,file="./data/recent4Data.RData")
                recent3Data<-process_message(input$text,ng=3)
                saveRDS(recent3Data,file="./data/recent3Data.RData")
                recent2Data<-process_message(input$text,ng=2)
                saveRDS(recent2Data,file="./data/recent2Data.RData")
                updateTextInput(session, "text",value = "")
        })
        
})