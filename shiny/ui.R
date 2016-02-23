library(shiny)
shinyUI(
ui <- fluidPage(
  
  titlePanel("Migration in EU countries"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "countries",label = "Choose countries",choices = countries),
      sliderInput(inputId="weather",label="Weather conditions",min=-1,max=2,value=0,step=0.1),
      sliderInput(inputId="conditions",label="EU migration politics",min=-1,max=2,value=0,step=0.1),
      sliderInput(inputId="war",label="Conditions on a battlefield",min=-1,max=2,value=0,step=0.1)
    ),
    mainPanel(
      tabsetPanel(tabPanel("Comparison",dataTableOutput("table"),plotOutput("plot"),
                           splitLayout(
                             plotOutput("age"),
                             plotOutput("origin"),
                             plotOutput("sex"))),
                  tabPanel("Prediction",plotOutput("prediction"))
      )
    )
  )
)
)