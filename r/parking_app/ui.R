library(shiny)

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
                   "Date:",
                   min = Sys.time()),
         
         uiOutput("time"),
         uiOutput("rugby"),
         uiOutput("homewin"),
         uiOutput("weather")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         uiOutput("things")
         #textOutput("precip")
      )
   )
)

