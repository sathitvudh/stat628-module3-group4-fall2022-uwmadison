library(shiny)
library(shinythemes)
library(tidyverse)

bigrams <- read.csv("bigrams.csv")
test <- read_csv("../../data/review/test.csv")

ui <- fluidPage(
  titlePanel("Suggestions for Bar Business Owners"),
  theme = shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(
      textInput("business", "Please enter your unique 22 character business id",value ="EZc2myE2mYk2h9JK9qu8gw")
    ),
    mainPanel(
      tabsetPanel(tabPanel("General Information",
      fluidRow(
        column(width = 12, h4("How to Use this App"),
               p("This purpose of this app is to provide data driven suggestions to help improve your business."),
               p("Below you will see a boxplot that gives you an idea where you fall in respect to other businesses"),
               p("If you run into any issues please contact Abby Terzis at terzis@wisc.edu for resolution")),
      ),
      fluidRow(
        column(width=6, h4("Where your bar falls compared to others"),plotOutput("boxplot")),
        column(width=6, h4("Phrases in highly rated reviews"),tableOutput("relavence")) 
      )
    ),
    tabPanel("Suggestions",
             h4("Put model suggestions here"))
    ),
  )
)
)


server <- function(input, output){
      
    output$boxplot <-renderPlot({ggplot(test, aes(x=test$keyword, y=test$`sentiment_score_%`)) + 
        geom_boxplot() + labs(x = "Main Categories", y = "Sentiment Score") +
        geom_point(data = test, aes(x=test$keyword,y=test$))
    })
      
    output$relavence <- renderTable({
      bigrams %>%
        filter(business_id == input$business) %>%
        select(bigram) %>%
        slice(1:5)
        
    })
      
  
      
   
}



shinyApp(ui, server)