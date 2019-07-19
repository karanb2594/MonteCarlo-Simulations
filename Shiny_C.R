library(shiny)
library(quantmod)
library(xts)
library(ggplot2)
library(tseries)

ui <- fluidPage(
  titlePanel("Monte Carlo Simulations"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "Days",
                   label = "Number of Days",
                   value = 30,
                   min = 5,
                   max = 5040,
                   step = 5),
      numericInput(inputId = "InitialInvestment",
                   label = "Amount of Investment",
                   value = 100000,
                   min = 25000,
                   max = 1000000000,
                   step = 1),
      textInput(inputId = "Ticker1",
                label = "Enter the first ticker",
                value = "AAPL"),
      numericInput(inputId = "Weight1",
                   label = "Enter the Weight",
                   value = 10,
                   min = 0,
                   max = 100,
                   step = 1),
      textInput(inputId = "Ticker2",
                label = "Enter the second ticker",
                value = "MSFT"),
      numericInput(inputId = "Weight2",
                   label = "Enter the Weight",
                   value = 20,
                   min = 0,
                   max = 100,
                   step = 1),
      textInput(inputId = "Ticker3",
                label = "Enter the third ticker",
                value = "XOM"),
      numericInput(inputId = "Weight3",
                   label = "Enter the Weight",
                   value = 30,
                   min = 0,
                   max = 100,
                   step = 1),
      textInput(inputId = "Ticker4",
                label = "Enter the fourth ticker",
                value = "GE"),
      numericInput(inputId = "Weight4",
                   label = "Enter the Weight",
                   value = 20,
                   min = 0,
                   max = 100,
                   step = 1),
      textInput(inputId = "Ticker5",
                label = "Enter the fifth ticker",
                value = "SBUX"),
      numericInput(inputId = "Weight5",
                   label = "Enter the Weight",
                   value = 20,
                   min = 0,
                   max = 100,
                   step = 1)),
    mainPanel(
      
      plotOutput("graph"),
      tableOutput("table1"),
      tableOutput("table2"),
      tableOutput("table3"),
      tableOutput("table4")
    )
  )
)
server1 <- function(input,output)
{
  output$graph <- renderPlot({myfunction(input$Ticker1,input$Ticker2,input$Ticker3,input$Ticker4,input$Ticker5,input$Weight1,input$Weight2,input$Weight3,input$Weight4,input$Weight5,input$InitialInvestment,input$Days)[1]})
output$table1 <- renderTable({myfunction(input$Ticker1,input$Ticker2,input$Ticker3,input$Ticker4,input$Ticker5,input$Weight1,input$Weight2,input$Weight3,input$Weight4,input$Weight5,input$InitialInvestment,input$Days)[2]})
output$table2 <- renderTable({myfunction(input$Ticker1,input$Ticker2,input$Ticker3,input$Ticker4,input$Ticker5,input$Weight1,input$Weight2,input$Weight3,input$Weight4,input$Weight5,input$InitialInvestment,input$Days)[3]})  
output$table3 <- renderTable({myfunction(input$Ticker1,input$Ticker2,input$Ticker3,input$Ticker4,input$Ticker5,input$Weight1,input$Weight2,input$Weight3,input$Weight4,input$Weight5,input$InitialInvestment,input$Days)[4]})
output$table4 <- renderTable({myfunction(input$Ticker1,input$Ticker2,input$Ticker3,input$Ticker4,input$Ticker5,input$Weight1,input$Weight2,input$Weight3,input$Weight4,input$Weight5,input$InitialInvestment,input$Days)[5]})
}