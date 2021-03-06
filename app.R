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
                                 tabPanel("Data", tableOutput("Horsedata")),
                                 tabPanel("Summary", verbatimTextOutput("Horsesummary")),
                                 tabPanel("Table", tableOutput("Horsetable"))
                     )
                 ))),
                 tabPanel("Univariate Plots",
                          fluidPage(
                            sidebarLayout(sidebarPanel(
                              selectInput("select", label = h3("Choose variable"), 
                                          choices = list(Price= "Price",Age="Age",Height="Height",Sex="Sex"),
                                          selected = 1)
                            ), mainPanel(
                              h3("Bar plot"),
                              plotOutput(outputId = "bar")
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
                            )))
                 #  titlePanel
) # navbarPage



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Horsedata <- renderTable(skim(HorsePrices))
  
  output$Horsesummary <- renderPrint(summary(HorsePrices))
  
  output$Horsetable <- renderTable(HorsePrices)
  
  output$bar <- renderPlot({
    ggplot(HorsePrices)+ xlab("")+
      geom_boxplot(aes(input$select))+ theme_minimal()
  })
  
  output$scatter <- renderPlot({
    ggplot(HorsePrices,aes(x= input$select2,y=Price,colour=Sex))+ xlab("")+
      geom_jitter()+ theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

