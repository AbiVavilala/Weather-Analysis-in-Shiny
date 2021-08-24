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
library(ggthemes)
library(shinythemes)
library(lubridate)
library(bslib)
library(plotly)
urlfile <- "https://raw.githubusercontent.com/AbiVavilala/Weather-Analysis-in-Shiny/main/weatherAUS.csv"

weather <- read.csv(url(urlfile))
weather$Date <- as.Date(weather$Date)
weather$year <- year(weather$Date)
weather$month <- months(weather$Date)
weather$month <- factor(weather$month, levels = month.name)
weather$WindGustDir <- NULL
weather$WindDir9am <- NULL
weather$WindDir3pm <- NULL
Location <- unique(weather$Location)
weather$temp_interval <- cut(weather$MaxTemp, breaks = seq(0,50, by = 3))
sydney_weather <- weather %>% filter(Location == "Sydney")
 
ui <- fluidPage( 
    theme = shinytheme("cerulean"),
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css.map")
    ),
    
    titlePanel("Weather Analysis in Australia from 2007 to 2017"),
    
    
    navbarPage(
        #theme = shinytheme("superhero"), 
        title = "Climate change is real",
        tabPanel(icon("home"),
                 
                 fluidRow(column(tags$img(src = "download.png", width= 200,height= 200),width=2),
                 column(
                     
                     br(),
                     
                     
                    tags$h4("Aim of this application is to show people Temperature is rising on Earth and it will affect societies and ecosystem. Some regional changes in Australian rainfall have been linked to humaninduced climate change. Southwest Western Australia has experienced a reduction in rainfall since 1970s and this has been attributed to change in surface temperature. I will use the recorded temperature in Australia and will show you how Australia is getting hot every year. Please see the recorded Temperature in Australia and make your mind about climate change.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px", width = 8),
                    
                      br(),
                     tags$h4("This app will analyse about 10 years of daily weather observations from many locations across Australia.",br(strong("Source & Acnowledgements: ")),  "Observations were drawn from 52 Weather stations from across Australia by Australian Government Bureau of Meteorology.", br(), "I would like to thank Joe Young for wranling data and making it available for public to download from Kaggle.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),width=8,),
                     column(tags$img(src = "download1.jpg", width = 200, height = 200), 
                         
                     
                      br(),
                      br(),
                      p("More information can be found on Australian Government Bureau of Meteorology page",
                        br(),
                        a(href= "http://www.bom.gov.au", " Clicking Here", target = "_blank"), style= "text-align:center;color:black"), width = 2),
                 br(),
                 br(),
                 br(),
                 
                 hr(),
                 
                 fluidRow(
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     
                 
                 
                     column(4,
                     h2(em("Developed by"),br("Abilash Rao Vavilala"),style="text-align:center; font-family: times"), offset = 5
                 
                 ))
                 )),
        
        
    tabPanel(title = "Temperature from 2007 to 2017",
             
            wellPanel( fluidRow(column(width=2),
                      column(
                          h4(p("Recorded Temperature in Australia from 2007 to 2017",style="color:black;text-align:center")),
                          width=8,style="background-color:lavender;border-radius: 10px")
             ),
             br(),
            
             fluidRow(
                 column(width=2, icon("hand-point-down","fa-5x"),align="center"),
                 column(
                     h4(p("We have data for Maximum and Minimum temperatre recorded in Australia. You can select the location and Scatterplot will be displayed. this plot will plot Maximum and Min Temperature in selected location from 2007 to 2017. Will also provide report for weather recorded in that location", style="color:black;text-align:justify")),
                     width=8,style="background-color:papayawhip;border-radius: 10px")
                
             ),
             
              fluidRow(
             column(
            
                 selectInput("Location", "Select a Location:", choices = Location , selected = Location[11]), 
                 actionButton(inputId = "clicks", label = "Run"), width = 8))),
            hr(),
            br(),
             fluidRow(
            plotlyOutput("point"),
            verbatimTextOutput("maxtemp"))),
    tabPanel(title = "Recorded temperature during days", 
             
             wellPanel(fluidRow(column(width=2),
                      column(
                          h4(p("Temperature Recorded during the day",style="color:black;text-align:center")),
                          width=8,style="background-color:lavender;border-radius: 10px")
             ),
             br(),
             fluidRow(
                 column(width=2, icon("hand-point-down","fa-5x"),align="center"),
                 column(
                     h4(p("In Dataset we have Recorded Temperature during the day. Temperature is clubbed in 3 degree interval. now we can see how many days recorded temperarure in the following interval. for example: 791 days in Sydney recorded temperature in the interval of (24, 27) in Ten year period.", style="color:black;text-align:justify")),
                     width=8,style="background-color:papayawhip;border-radius: 10px")
                 
             ),
            fluidRow( 
                column(
             selectInput("Location1", "Select a Location: ", choices = Location, selected = Location[11]),
             actionButton(inputId = "tabtwo_click", label = "Run"), width =8))),
            hr(),
            br(),
             plotOutput("point1")), 
    tabPanel(title = "Yearly analysis",
            wellPanel( fluidRow(column(width=2),
                      column(
                          h4(p("Yearly analysis",style="color:black;text-align:center")),
                          width=8,style="background-color:lavender;border-radius: 10px")
             ),
             br(),
             fluidRow(
                 column(width=2, icon("hand-point-down","fa-5x"),align="center"),
                 column(
                     h4(p("To understand change in Weather in Australia let's look at yearly analysis. please selct the year and location, you can see Scatterplot of Max and Min temperature recorded in that location for that year and Weather report for selected Criteria will be provided. For Example: Average Temp in Sydney in 2008 is 21.73 and Average Temp in 2017 is 24.68. Avg Temp for Sydney has only increased every year.", style="color:black;text-align:justify")),
                     width=8,style="background-color:papayawhip;border-radius: 10px")
                 
             ),
             fluidRow(
             column(
             selectInput("Year", "Select a Year: ", unique(weather$year)),
             selectInput("Location2", "Location: ", unique(weather$Location)),
             actionButton(inputId = "tabthree_click", label = "Run"), width =8))),
             hr(),
             br(),
             plotlyOutput("point2"),
             tags$h2(verbatimTextOutput("sum"))
    
    

                 
                 
                 
                 
                 
                 
                 )))

server <- function(input, output){
     
    
    tabone_data <- eventReactive(input$clicks, {
        weather %>% filter(Location == input$Location)
        
    })
    
    tabtwo_data <- eventReactive(input$tabtwo_click, {
         weather %>% filter(Location == input$Location1)
        
    })
    
    data <- eventReactive(input$tabthree_click, {
        weather %>% filter(Location == input$Location2 & year == input$Year)
        
    })
    output$point <- renderPlotly({
        
        p <- ggplot(data = tabone_data(), aes(MaxTemp, MinTemp)) + geom_point() + ggtitle(" recorded Temperature in between 2007 and 2017") + theme_economist()
        ggplotly(p)
    })
    output$point1 <- renderPlot({
        
        ggplot(data = tabtwo_data(), aes(temp_interval, fill = temp_interval)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) + ggtitle("Number of Days recorded Temperature") + xlab("Temperature Interval ") + ylab("Number of days")
        
    })
    output$point2 <- renderPlotly({
        
        ggplot(data = data(), aes(MaxTemp, MinTemp)) + geom_point() 
        
    })
    output$sum <- renderPrint({
        summary(data())
    })
    
    output$maxtemp <- renderPrint({
        summary(tabone_data())
    })
    
}

shinyApp(ui = ui, server = server)
