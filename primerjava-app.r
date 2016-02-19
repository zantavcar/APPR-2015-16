library(shiny)
ui <- fluidPage(
  titlePanel("Asylum seekers in EU"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "countries",label = "Choose countries",choices = countries)
      ),
    mainPanel(
      dataTableOutput("table"),
      plotOutput("plot")
    )
  )
)



server <- function(input, output) {
  output$table <- renderDataTable({tabela_Prosnje %>% filter(GEO %in% input$countries)})
  output$plot <- renderPlot({ggplot(tidy_Prosnje2015 %>% filter(ASYL_APP == "Asylum applicant",
                                                                GEO %in% input$countries),
                                    aes(x = TIME, y = Value, group = GEO, color = GEO))+geom_line()})
  
}

shinyApp(ui = ui, server = server)