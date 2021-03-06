#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(Stat2Data)
library(skimr)

data(HorsePrices)

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app Horse Prices",
                 tabPanel("Data Description",
                   fluidPage(
                   mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Table", tableOutput("Horsetable")),
                                 tabPanel("Description", tableOutput("Horsedata")),
                                 tabPanel("Summary", verbatimTextOutput("Horsesummary"))
                                 
                     )
                 ))),
                 tabPanel("Univariate Plots",
                          fluidPage(
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("Price", plotOutput("price")),
                                                  tabPanel("Age", plotOutput("age")),
                                                  tabPanel("Height", plotOutput("height")),
                                                  tabPanel("Sex", plotOutput("sex"))
                            )
                            
                            ))),
                            
                 tabPanel("Bivariate Plots",
                          fluidPage(
                            sidebarLayout(position = "right",
                              sidebarPanel(
                                 selectInput("select2", label = h3("Choose variable"), 
                                          choices = list(Age="Age",Height="Height",HorseID="HorseID"),
                                          selected = 1)
                            ), mainPanel(
                              h3("Scatter plot Price by Sex"),
                              plotOutput(outputId = "scatter")
                            )
                            ))),
                 tabPanel("Regression model",
                          fluidPage(
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Multiple Regression Summary", verbatimTextOutput("Modelsummary")),
                                          tabPanel("Price ~ Age", plotOutput("Modelplot1")),
                                          tabPanel("Price ~ Height", plotOutput("Modelplot2")),
                                          tabPanel("Price by Sex", plotOutput("Modelplot3"))
                              )
                            ))),
                 tabPanel("Predict new horse price",
                          fluidPage(
                            sidebarLayout(sidebarPanel(
                            sliderInput("Age", label = h3("Age of horse"), min = 0, 
                                        max = 25, value = 10),
                            sliderInput("Height", label = h3("Height of horse"), min = 14, 
                                        max = 20, value = 16),
                            selectInput("Sex", label = h3("Sex"),choices =c(Female="f",Male="m"))
                            ),
                            
                            mainPanel(
                              h3("The price of the new horse is:"),
                              textOutput("predict")
                            )
                          )
                 ))
                 
                 #  titlePanel
) # navbarPage



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Horsedata <- renderTable(skim(HorsePrices))
  
  output$Horsesummary <- renderPrint(summary(HorsePrices))
  
  output$Horsetable <- renderTable(HorsePrices)
  
  output$price <- renderPlot({
    ggplot(HorsePrices)+ 
      geom_boxplot(aes(Price),fill="tomato2")+ coord_flip()+ theme_minimal()+xlab("Price")
  })
  output$height <- renderPlot({
    ggplot(HorsePrices)+
      geom_boxplot(aes(Height),fill="gold3")+ coord_flip()+ theme_minimal()+xlab("Height")
  })
  output$sex <- renderPlot({
    ggplot(HorsePrices, aes(fill=Sex))+ xlab("Sex")+
      geom_bar(aes(Sex))+ theme_minimal()
  })
  output$age <- renderPlot({
    ggplot(HorsePrices)+ 
      geom_density(aes(Age),fill="orange",alpha=0.2)+ theme_minimal()+xlab("Age")
  })
  
  output$scatter <- renderPlot({
    ggplot(HorsePrices,aes(x= input$select2,y=Price,colour=Sex))+ xlab("")+
      geom_jitter()+ theme_minimal()
  })
  
  output$Modelsummary <- renderPrint(summary(lm(Price~Age+Height+Sex,HorsePrices)))
  
  
  output$Modelplot1 <- renderPlot({
    ggplot(HorsePrices,aes(x= Age,y=Price))+ 
      geom_jitter()+ geom_smooth()+ theme_minimal()
  })
  
  output$Modelplot2 <- renderPlot({
    ggplot(HorsePrices,aes(x= Height,y=Price))+ 
      geom_jitter()+ geom_smooth()+ theme_minimal()
  })
  
  output$Modelplot3 <- renderPlot({
    ggplot(HorsePrices,aes(x= Sex,y=Price,col=Sex))+ 
      geom_boxplot()+ theme_minimal()
  })
  
  output$predict <- renderText(predict(lm(Price~Age+Height+Sex,HorsePrices),data.frame(Age=input$Age, Height=input$Height, Sex=input$Sex)))
}

# Run the application 
shinyApp(ui = ui, server = server)

runGitHub("R-Shiny","juanmiRL")
