suppressPackageStartupMessages(c(
        library(shinythemes),
        library(shiny),
        library(tm),
        library(stringr),
        library(markdown),
        library(stylo)))

shinyUI(
        
        fluidPage(
                
                title="Coursera Data Science Capstone",
                titlePanel("Coursera Data Science Capstone"),
                theme = shinytheme("journal"),
                fluidRow(column(2,
                                h4("Suggested words:"),
                                uiOutput("word1"),
                                uiOutput("word2"),
                                uiOutput("word3"),
                                h4("Recently used:"),
                                uiOutput("wordR"),
                                h4("Suggested by Google:"),
                                uiOutput("wordG")
                ),
                column(6,
                       tags$div(
                               tags$textarea(id="text", rows=30, cols=400, ""),
                               tags$style(type='text/css', 
                                          "#text { width: 400px;height: 300px; }",
                                          "#foo:focus { border: 2px solid red; }",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"),
                               
                               br(),
                               actionButton("send", "Send"),
                               br(),
                               tags$hr()
                       )
                )
                )
        ))

