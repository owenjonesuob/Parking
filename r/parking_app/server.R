`%>%` <- dplyr::`%>%`
library(shiny)

server <- function(input, output) {
    
    output$time <- renderUI({
        
        sliderInput("time", "Time:",
                    min = {
                        if (input$date == Sys.Date()) {
                            lubridate::ymd("2000-01-01") + 
                                lubridate::hm(substr(Sys.time(),
                                                     12, 16))
                        } else {
                            lubridate::ymd_hm("2000-01-01 00:00")}
                    },
                    max = lubridate::ymd_hm("2000-01-01 23:59"),
                    value = lubridate::ymd_hm("2000-01-01 12:00"),
                    step = 60, timeFormat = "%H:%M")
    })
    
    dtime <- reactive(
        as.character(lubridate::hour(input$time) + lubridate::minute(input$time)/60)
    )
    
    wd <- reactive(
        as.character(lubridate::wday(input$date))
    )
    
    wk <- reactive(
        as.character(lubridate::week(input$date))
    )
    
    
    output$rugby <- renderUI({
        checkboxInput("rugbytick",
                      "Is there a rugby match on?",
                      value = tryCatch({
                          fixtures <- xml2::read_html("https://www.bathrugby.com/fixtures-results/fixtures/1st-xv-fixtures/") %>%
                              rvest::html_nodes(xpath = "//dd[contains(@class, 'homeFixture')]/span[@class='fixtureDate']") %>%
                              rvest::html_text() %>%
                              lubridate::as_date(format = "%n%e %b %Y")
                          
                          input$Date %in% fixtures
                          
                      }, error = function(e) FALSE
                      )
        )
    })
    
    output$homewin <- renderUI({
        req(input$rugbytick)
        checkboxInput("wintick",
                      "Are Bath going to win?",
                      value = TRUE)
    })

    
    ct <- 3
    
    
    
    output$weather <- renderUI({
        checkboxGroupInput("precip",
                           "Is it likely to be:",
                           choices = c("rainy?",
                                       "snowy?",
                                       "foggy?"),
                           
                           selected = {
                               tryCatch({
                                   weather <- xml2::read_html("https://weather.com/en-GB/weather/tenday/l/Bath+BAS+United+Kingdom+UKXX0012:1:UK") %>%
                                       rvest::html_table(fill = TRUE, header = FALSE) %>%
                                       .[[1]]
                                   names(weather) <- c(NA, weather[1, 1:6])
                                   weather <- weather[-1, ] %>%
                                       dplyr::mutate(Day = sapply(Day, function(x) {
                                           daymonth <- substr(x, nchar(x)-5, nchar(x))
                                           lubridate::as_date(daymonth, format = "%e %b")
                                       })) %>%
                                       dplyr::mutate(rain = (grepl("[Rr]ain", Description)),
                                                     snow = (grepl("[Ss]now", Description)),
                                                     fog = (grepl("[Ff]og", Description)))
                                   
                                   c(weather$rain[input$date],
                                     weather$snow[input$date],
                                     weather$fog[input$date])
                               }, error = function(e) c("rainy?")
                               )
                           }
        )
    })
    
        
    rainmm <- 1.55
    
    output$precip <- renderText(
        input$precip)
    
    rain <- reactive(
        input$precip[1]
    )
    
    snow <- reactive(
        input$precip[2]
    )
    
    fog <- reactive(
        input$precip[3]
    )
    
    #print(paste(carpark, dtime, wd, wk, rug, HomeWin, ct, snow, rain, fog, rainmm,
    #           sep = "<br/>"))
    
    
    
    
    output$things <- renderUI(
        HTML(paste(input$carpark, dtime(), wd(), wk(), input$rugbytick,
                   input$wintick, rain(), snow(), fog(),
                   sep = "<br/>"))
    )
    
}