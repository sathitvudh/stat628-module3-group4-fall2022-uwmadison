#install.packages("shiny")
#install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(tidyverse)

#runExample("02_text")
#runExample("03_reactivity")
#runExample("04_mpg") 
#runExample("06_tabsets")

bars_list <- read.csv("../../data/bars_list_ca.csv")
bigrams <- read.csv("../Shiny/bigrams2.csv")
top_five <- read.csv("../../data/Review/top5_analysis.csv")
sentiment_all <- read_csv("../Shiny/final_review_sentiment.csv")
sentiment_main <- sentiment_all %>%
  filter(keyword == "food" | keyword == "drink" | keyword == "price")

ui <- fluidPage(
  titlePanel("Suggestions for Bar Business Owners"),
  theme = shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(
      textInput("business", "Please enter your business id",value ="EZc2myE2mYk2h9JK9qu8gw"),
      br(),
      h5(strong("How to use this App"), align="center"),
      p("1. Enter your unique 22-character business id"),
      p("2. General information tab: Your business overview"),
      p("3. Suggestions tab: Our recommendation to improve your business"),
      br(),
      p("This purpose of this app is to provide data driven suggestions to help improve your business."),
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
                             column(width=6, h3(strong("Where your bar falls compared to others")),plotOutput("boxplot")),
                             column(width=6, h3("Top five most mentions"), h4(strong(htmlOutput("top5")))),
                             column(width=6, h3(strong("Phrases in highly rated reviews")),tableOutput("relavence"))
                           )
      ),
      tabPanel("Suggestions",
               h4("Put model suggestions here"))
      ),
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
  
  output$top5 <- renderUI({
    top5 = top_five %>%
      filter(business_id == input$business) %>%
      select(word_list)
  })
  
  
  
  
}



shinyApp(ui, server)