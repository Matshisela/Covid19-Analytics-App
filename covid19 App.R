###############################################################################
#               Covid19 Analytics App
#       R-Shiny Application
###############################################################################


# Library Packages ----
library(covid19.analytics)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(plotly)


# Define UI ----
shinyUI <- dashboardPage(skin = "green",
                         dashboardHeader(title = "Covid19 Analytics"),
                         dashboardSidebar(
                           
                           sidebarMenu(
                             selectInput(inputId = "category", label = "Select Category",
                                         choices = c("Confirmed", "Recovered", "Deaths", "All")),
                             selectInput(inputId = "country", label = "Select specific country",
                                         choices = NULL)),
                             
                             
                             menuItem("Report Summary", tabName = "Reports",
                                      icon = icon("calculator")),
                             menuItem("Statistical Models", tabName = "StatsModels",
                                      icon =  icon("calculator")),
                             menuItem("Trends", tabName = "trendz", 
                                      icon =  icon("chart-line")),
                             menuItem("Growth Rate", tabName = "growth",
                                      icon = icon("chart-line"))
                             
                           ),
                         
                         dashboardBody(
                           tabItems(
                             tabItem(tabName = "Reports",
                                     verbatimTextOutput("summary1")),
                             tabItem(tabName = "StatsModels",
                                     verbatimTextOutput("summary2")),
                             tabItem(tabName = "trendz",
                                     #tags$b("Confirmed cases in the Country"),
                                     box(width = 10,
                                         plotOutput("plot1", height = "400px")),
                                     br(),
                                     #tags$b("Cases for each parameter"),
                                     box(width = 12,
                                         plotlyOutput("plot2", height = "500px"))),
                             tabItem(tabName = "growth",
                                     box(width = 12,
                                         plotOutput("plotl3", height = "750px")))
                           )
                         )
                         
  )


shinyServer <- function(input, output, session){
  
  observe({
    updateSelectInput(session, 'country', choices = unique(covid19.data("ts-confirmed")$Country.Region))
  })
  
  
    output$summary1 <- renderPrint({
      report.summary(geo.loc = input$country, graphical.output = FALSE)
    })
    
    
    output$summary2 <- renderPrint({
      if (input$category == "Confirmed"){
        data <- covid19.data("ts-confirmed")
        tots.per.location(data, geo.loc = input$country)
      } else if (input$category == "Recovered"){
        data <- covid19.data("ts-recovered")
        tots.per.location(data, geo.loc = input$country)
      } else if (input$category == "Deaths") {
        data <- covid19.data("ts-deaths")
        tots.per.location(data, geo.loc = input$country)
      } else {
        data <- covid19.data("ts-All")
        tots.per.location(data, geo.loc = input$country)
      }
      
    })
    
    output$plot1 <- renderPlot({
      if (input$category == "Confirmed"){
        data <- covid19.data("ts-confirmed")
        single.trend(data[data$Country.Region == input$country, ])
      } else if (input$category == "Recovered"){
        data <- covid19.data("ts-recovered")
        single.trend(data[data$Country.Region == input$country, ])
      } else if (input$category == "Deaths") {
        data <- covid19.data("ts-deaths")
        single.trend(data[data$Country.Region == input$country, ])
      } else {
        data <- covid19.data("ts-All")
        return()
      }
      
      
    })
    
    
    output$plot2 <- renderPlotly({
      if (input$category == "Confirmed"){
        data <- covid19.data("ts-confirmed")
        itrends(data, geo.loc = input$country)
      } else if (input$category == "Recovered"){
        data <- covid19.data("ts-recovered")
        itrends(data, geo.loc = input$country)
      } else if (input$category == "Deaths") {
        data <- covid19.data("ts-deaths")
        itrends(data, geo.loc = input$country)
      } else {
        data <- covid19.data("ts-ALL")
        itrends(data, geo.loc = input$country)
      }
      
      
    }) 
      
    output$plot3 <- renderPlot({
      if (input$category == "Confirmed"){
        data <- covid19.data("ts-confirmed")
        growth.rate(data, geo.loc = input$country)
      } else if (input$category == "Recovered"){
        data <- covid19.data("ts-recovered")
        growth.rate(data, geo.loc = input$country)
      } else if (input$category == "Deaths") {
        data <- covid19.data("ts-deaths")
        growth.rate(data, geo.loc = input$country)
      } else {
        data <- covid19.data("ts-ALL")
        return()
      }
      
      
    })   
}

shinyApp(shinyUI, shinyServer)
  