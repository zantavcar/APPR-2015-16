library(shiny)
ui <- fluidPage(
  titlePanel("Asylum seekers in EU"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "countries",label = "Choose countries",choices = countries)
      ),
    mainPanel(
      dataTableOutput("table"),
      plotOutput("plot"),
      plotOutput("age")
    )
  )
)



server <- function(input, output) {
  output$table <- renderDataTable({tabela_Prosnje %>% filter(GEO %in% input$countries)})
  output$plot <- renderPlot({ggplot(tidy_Prosnje2015 %>% filter(ASYL_APP == "Asylum applicant",
                                                                GEO %in% input$countries),
                                    aes(x = TIME, y = Value, group = GEO, color = GEO))+geom_line()+
                                    ggtitle("Number of asylum applicants by month in selected countries")})
  output$age <- renderPlot({ggplot(starost %>% filter(GEO %in% input$countries)) + 
                                               aes(x=AGE,y=applicants,fill=GEO,color=GEO)+ 
                                               geom_bar(stat="identity",position=position_dodge())+ 
                                               theme(axis.text.x = element_text(angle = 70, vjust = 0.5))+
                                               ggtitle("Age of asylum seekers") })
  
}

shinyApp(ui = ui, server = server)