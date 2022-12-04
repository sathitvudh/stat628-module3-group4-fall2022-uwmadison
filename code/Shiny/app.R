
library(shiny)

ui <- fluidPage(
  selectInput("s1","Businessparking is validated",choices = c("True","False")),
  selectInput("s2","Businessparking has valet",choices = c("True","False")),
  selectInput("s3","Lunch is good for meal",choices = c("True","False")),
  selectInput("s4","Ambience includes touristy",choices = c("True","False")),
  selectInput("s5","Music includes Karaoke",choices = c("True","False")),
  selectInput("s6","Alcohol",choices = c("full_bar","beer_and_wine")),
  selectInput("s7","Ambience includes intimate",choices = c("True","False")),
  selectInput("s8","Ambience includes hipster",choices = c("True","False")),
  selectInput("s9","Businessparking has street",choices = c("True","False")),
 textOutput("result")
)

server <- function(input, output) 
{
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

}

shinyApp(ui, server)