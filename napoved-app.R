library(shiny)
ui <- fluidPage(
  titlePanel("Migration - Predictions in EU countries"),
  sidebarLayout(
    sidebarPanel(
      SelectInput(inputId = "countries",label = "Choose countries",choices = countries),
      SliderInput(inputId ="weather",label= "Weather conditions",min=0,max=2,value=1),
      SliderInput(inputId ="politics",label= "Migartion politics of EU countries",min=0,max=2,value=1),
      SliderInput(inputId ="war",label= "Conditions on battlefield",min=0,max=2,value=1)
      
    ),
    mainPanel(
      dataTableOutput("table"),
      splitLayout(
      plotOutput("sex"),
      plotOutput("age"),
      plotOutput("origin")),
      plotOutput("predictions"))
      
    )
  )


server <- function(input, output) {
  output$table <- renderDataTable({tabela_Prosnje %>% filter(GEO %in% input$countries)},
                                  options = list(searching = FALSE, paging = FALSE))
  
}

shinyApp(ui = ui, server = server)