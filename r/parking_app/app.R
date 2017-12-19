#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Parking"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("carpark", 
                     "Car park:",
                     choices = c("Avon Street CP", "Charlotte Street CP", 
                                 "Lansdown P+R", "Newbridge P+R", "Odd Down P+R", "Podium CP", 
                                 "SouthGate General CP", "SouthGate Rail CP")),
         dateInput("date",
                   "Date:"),
         selectInput("hour",
                     "Hour:",
                     choices = 0:23),
         selectInput("minute",
                     "Minute:",
                     choices = 0:60)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         uiOutput("things")
      )
   )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(plumber)
    
    library(h2o)
    h2o.init()
    h2o_rf <- h2o.loadModel(paste0(getwd(), "/model/DRF_model_R_1513191421362_1.rf"))
    
    
    #* @post /carpark
    carpark <- reactive(
        input$carpark
    )
    
    #* @post /timeOfDay
    timeOfDay <- reactive(
        as.character(as.numeric(input$hour) + as.numeric(input$minute)/60)
    )
    
    #* @post /weekday
    weekday <- reactive(
        as.character(lubridate::wday(input$date))
    )
    4
    
    #* @post /weekNumber
    weekNumber <- reactive(
        as.character(lubridate::week(input$date))
    )
    
    
    
    fixtures <- xml2::read_html("https://www.bathrugby.com/fixtures-results/fixtures/1st-xv-fixtures/") %>%
        rvest::html_nodes(xpath = "//dd[contains(@class, 'homeFixture')]/span[@class='fixtureDate']") %>%
        rvest::html_text() %>%
        lubridate::as_date(format = "%n%e %b %Y")
    
    
    #* @post /rugby
    rugby <- reactive(
        input$Date %in% fixtures
    )
    
    #* @post /rugbyHomeWin
    rugbyHomeWin <- 0.5
    
    #* @post /cityEvents
    cityEvents <- 3
    
    
    
    
    weather <- xml2::read_html("https://weather.com/en-GB/weather/tenday/l/Bath+BAS+United+Kingdom+UKXX0012:1:UK") %>%
    rvest::html_table(fill = TRUE, header = FALSE) %>%
        .[[1]]
    names(weather) <- c(NA, weather[1, 1:6])
    weather <- weather[-1, ] %>%
        mutate(Day = sapply(Day, function(x) {
            daymonth <- substr(x, nchar(x)-5, nchar(x))
            lubridate::as_date(daymonth, format = "%e %b")
        })) %>%
        mutate(snow = (grepl("[Ss]now", Description)),
               rain = (grepl("[Rr]ain", Description)),
               fog = (grepl("[Ff]og", Description)))
    
    #* @post /snow
    snow <- reactive(
        weather$snow[input$date]
    )
    
    #* @post /rain
    rain <- reactive(
        weather$rain[input$date]
    )
    
    #* @post /fog
    fog <- reactive(
        weather$fog[input$date]
    )
    
    #* @post /precipitation
    precipitation <- 1.55
    
    
    to_post <- list("carpark" = carpark,
                    "timeOfDay" = timeOfDay,
                    "weekday" = weekday,
                    "weekNumber" = weekNumber,
                    "rugby" = rugby,
                    "rugbyHomeWin" = rugbyHomeWin,
                    "cityEvents" = cityEvents,
                    "weather" = list(
                        "rain" = rain,
                        "snow" = snow,
                        "fog" = fog,
                        "precipitation" = precipitation))
                    
    
    library(jsonlite)
    to_post_json <- toJSON(to_post)
    
    
    
    output$things <- renderUI(
        HTML(paste(to_post_json))
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

