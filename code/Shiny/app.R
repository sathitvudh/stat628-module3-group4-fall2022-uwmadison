library(shiny)
library(shinythemes)
library(tidyverse)

# Filter top 5 words for each business
bigrams <- read_csv("bigrams.csv") 



ui <- fluidPage(
  titlePanel("Bar Analysis"),
  theme = shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(
      textInput("business", "Business ID"),
      helpText("Please enter your 22 character business id"),
      helpText("If you run into any issues please contact Abby Terzis at terzis@wisc.edu for resolution")
    ),
    mainPanel(
      tabsetPanel(tabPanel("General Information",
      fluidRow(
        column(width = 6, helpText("general info here")),
      ),
      fluidRow(
        column(width=6, helpText("boxplot here"),plotOutput("boxplot")),
        column(width=6, helpText("suggestions here"),textOutput("frequency")), 
      )
    ),
    tabPanel("Attribute Analysis",
             helpText("Put model suggestions here"))
    ),
  )
)
)



server <- function(input, output){
  output$frequency <- renderText({
    paste0("Put suggestions here ")
  })
}

shinyApp(ui, server)