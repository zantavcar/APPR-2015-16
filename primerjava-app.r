library(shiny)
library(mgcv)
options(scipen=999)
ui <- fluidPage(
  
  titlePanel("Migration in EU countries"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "countries",label = "Choose countries",choices = countries),
      sliderInput(inputId="weather",label="Weather conditions",min=-1,max=2,value=1,step=0.1),
      sliderInput(inputId="conditions",label="EU migration politics",min=-1,max=2,value=1,step=0.1),
      sliderInput(inputId="war",label="Conditions on a battlefield",min=-1,max=2,value=1,step=0.1)
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
server <- function(input, output) {
  output$table <- renderDataTable({tabela_Prosnje %>% filter(GEO %in% input$countries)},
                                  options = list(searching = FALSE, paging = FALSE))
  output$plot <- renderPlot({ggplot(tidy_Prosnje2015 %>% filter(ASYL_APP == "Asylum applicant",
                                                                GEO %in% input$countries),
                                    aes(x = TIME, y = Value, group = GEO, color = GEO))+geom_line()+
                                    ggtitle("Number of asylum applicants by month in selected countries")+
                                    xlab("")+ylab("")})
  output$age <- renderPlot({ggplot(starost %>% filter(GEO %in% input$countries)) + 
                                               aes(x=AGE,y=applicants,fill=GEO,color=GEO)+ 
                                               geom_bar(stat="identity",position=position_dodge())+ 
                                               theme(axis.text.x = element_text(angle = 70, vjust = 0.5))+
                                               ggtitle("Age of asylum seekers")+
                                               xlab("")+ylab("")})
  output$sex <- renderPlot({ggplot(spol %>% filter(GEO %in% input$countries)) + 
      aes(x=SEX,y=applicants,fill=GEO,color=GEO)+ 
      geom_bar(stat="identity",position=position_dodge())+ 
      theme(axis.text.x = element_text(angle = 70, vjust = 0.5))+
      ggtitle(paste0("Sex of asylum seekers in ","selected countries"))+xlab("")+ylab("")})
  output$origin <- renderPlot({ggplot(origin %>% filter(GEO %in% input$countries),
                                      aes(x=CITIZEN,y=applicants,fill=GEO,color=GEO))+ 
      geom_bar(stat="identity",position=position_dodge())+
      theme(axis.text.x = element_text(angle = 70, vjust = 0.5))+
      ggtitle(paste0("Origin of asylum seekers in ","selected countries"))+xlab("")+ylab("") })
  output$prediction <- renderPlot({ggplot(tidy_Prosnje2015 %>% filter(ASYL_APP == "Asylum applicant",GEO %in% input$countries) %>%
                                            mutate(applicants=Value+Value*(0.15*input$weather+0.7*input$conditions+0.15*input$war)),
                                          aes(x=MONTH,y=applicants,group=GEO,color=GEO))+geom_smooth(method = "loess")+
                                          xlab("")+ylab("")+
                                          ggtitle("Predictions of migrant flows")})
}

shinyApp(ui = ui, server = server)