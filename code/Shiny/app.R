library(shiny)
library(shinythemes)
library(tidyverse)


bars_list <- read.csv("bars_list_ca.csv")
bigrams <- read.csv("bigrams2.csv")
top_five <- read.csv("top5_analysis.csv")
sentiment_all <- read_csv("final_review_sentiment.csv")
sentiment_main <- sentiment_all %>%
  filter(keyword == "food" | keyword == "drink" | keyword == "price")

ui <- fluidPage(
  titlePanel("Suggestions for Bar Business Owners"),
  theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      textInput("business", "Please enter your business id",value ="EZc2myE2mYk2h9JK9qu8gw"),
      br(),
      h5(strong("How to use this App"), align="center"),
      p("1. Enter your unique 22-character business id"),
      p("2. General information tab: Your business overview"),
      p("3. Suggestions tab: Our recommendation to improve your business"),
      br(),
      p("The purpose of this app is to provide data driven suggestions to help improve your business."),
      br(),
      p("If you run into any issues please contact:"),
      p(" - Abby Terzis (terzis@wisc.edu)"),
      p(" - Samach Sathitvudh (sathitvudh@wisc.edu)"),
      p(" - Jianzhuo Liu (jliu2245@wisc.edu)")
    ),
    mainPanel(
      tabsetPanel(tabPanel("General Information",
                           fluidRow(
                             h3(strong(textOutput("name")),align="center"),
                             br(),
                             h4(strong(textOutput("stars")),align="center"),
                             column(width=6, h4("Where your bar falls compared to others:"),plotOutput("boxplot")),
                             column(width=6, h4("This is what people are raving about to others! Use this to market your bar through social media and/or posters."),
                                    tableOutput("relavence"),
                                    br(),
                                    h4("Top 5 Most Mentions"),
                                    h4(strong(htmlOutput("freq")))
      ))),
      tabPanel("Suggestions",
               fluidRow(
               column(width=6,
               selectInput("s1","Parking is validated",choices = c("True","False")),
               selectInput("s2","Valet Parking",choices = c("True","False")),
               selectInput("s3","Serve Lunch",choices = c("True","False")),
               selectInput("s4","Ambience is touristy",choices = c("True","False")),
               selectInput("s5","Karaoke",choices = c("True","False")),
               selectInput("s6","Type of Alcohol",choices = c("full_bar","beer_and_wine")),
               selectInput("s7","Ambience is intimate",choices = c("True","False")),
               selectInput("s8","Ambience is hipster",choices = c("True","False")),
               selectInput("s9","Street Parking",choices = c("True","False"))),
               column(width=6,
                      h3(textOutput("title")),
                      h4(textOutput("instruction1")),
                      br(),
               h4(textOutput("instruction")),
               br(),
               h1(textOutput("result")) ,
               br(),
               h4(textOutput("recommendations")),
               h4(textOutput("fix")),
               h4(textOutput("recommendations1")),
               h4(textOutput("recommendations2")),
               h4(textOutput("recommendations3")),
               h4(textOutput("recommendations4"))
               ))
      )
    ) 

)
)
)

server <- function(input, output){

  output$name <- renderText({
    name = bars_list %>%
      filter(business_id == input$business) %>%
      select(name) %>%
      slice(1)
    unlist(name[1,], use.names = FALSE)
  })

  output$stars <- renderText({
    stars = bars_list %>%
      filter(business_id == input$business) %>%
      select(stars_y) %>%
      slice(1)
    stars = unlist(stars[1,], use.names = FALSE)
    paste("You earn ", stars, " /5 stars", sep = "")
  })

  output$boxplot <-renderPlot({
    highlight = sentiment_main %>%
      filter(business_id == input$business) %>%
      select(c(business_id,keyword,`sentiment_score_%`))

    ggplot(sentiment_main, aes(x=keyword, y=`sentiment_score_%`)) +
      geom_boxplot() + labs(x = "Main Categories", y = "Sentiment Score") +
      geom_point(data = highlight, aes(x=keyword,y=`sentiment_score_%`), colour = "red", size = 4)

  })

  output$relavence <- renderTable({
    bigrams %>%
      filter(business_id == input$business) %>%
      select(bigram) %>%
      slice(1:5)
  })
  
  output$freq <- renderUI({
    top5 = top_five %>%
      filter(business_id == input$business) %>%
      select(word_list)
  })
  
  
  
  
  
  f <- reactive({
    x1 <- ifelse(input$s1 == "True", 1, 0)
    x2 <-ifelse(input$s2 == "True", 1, 0)
    x3 <- ifelse(input$s3 == "True", 1, 0)
    x4 <- ifelse(input$s4 == "True", 1, 0)
    x5 <-ifelse(input$s5 == "True", 1, 0)
    x6 <-ifelse(input$s6 == "full_bar", 1, 0)
    x7 <-ifelse(input$s7 == "True", 1, 0)
    x8 <-ifelse(input$s8 == "True", 1, 0)
    x9 <-ifelse(input$s9 == "True", 1, 0)
    x10 <-ifelse((input$s2 == "True") && (input$s3 == "True"), 1, 0)
    x11 <-ifelse((input$s4 == "True") && (input$s5 == "True"), 1, 0)
    x12 <-ifelse((input$s4 == "True") && (input$s6 =="full_bar"), 1, 0)
    x13 <-ifelse((input$s7 == "True") && (input$s8 == "True"), 1, 0)
    x14 <-ifelse((input$s7 == "True") &&(input$s9 == "True"), 1, 0)
    x15 <-ifelse((input$s3 == "True") && (input$s8 == "True"), 1, 0)
    x16 <-ifelse((input$s3 == "True") && (input$s6 == "full_bar"), 1, 0)
    r <- 1.483235 + 0.083382*x1  -0.078589*x3 -0.073331*x6 -0.145006*x8-0.156377*x10-0.178290*x11 +0.166333* x12 +0.143959* x13 -0.168094*x14 + 0.098175*x15 +0.086016* x16
    return(format(round(2.71828^(r),1),nsmall =1))
  })
  output$result <- renderText({
    paste("The predictive rating is", f())
  })
  output$instruction <- renderText({
    paste("2. Be sure to scroll all the way down!")
  })
  
  output$title <- renderText({
    paste("**Disclaimer**")
  })
  
  output$instruction1 <- renderText({
    paste("1. The fields on the left are pre-set and do not update automatically when you input your business ID, please adjust the fields according to the features of your bar.")
  })
  
  output$recommendations <- renderText({
    paste("Recommendations:")
  })
  output$fix <- renderText({
    paste(
      "If your bar validates parking, your rating will increase by 0.08")
  })
  output$recommendations1 <- renderText({
    paste(
      "If the ambience is touristy and you have a full bar, your rating will increase by 0.16")
  })
  output$recommendations2 <- renderText({
    paste(
      "If the ambience is intimate and hipster, your rating will increase by 0.14")
  })
  output$recommendations3 <- renderText({
    paste(
      "If your bar prepares good meals for lunch and the ambience is hipster, your rating will increase by 0.1")
  })
  output$recommendations4 <- renderText({
    paste(
      "If your bar prepares good meals for lunch and has a full bar, your rating will increase by 0.09")
  })
  
  


}



shinyApp(ui, server)
