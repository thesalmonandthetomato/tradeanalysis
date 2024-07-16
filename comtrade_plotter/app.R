
# look up HS code description
hs_codes <- read.csv('www/hscodes.csv', colClasses=c("hscode"="character"))
hscode_desc <- paste0(hs_codes$hscode, " - ", hs_codes$description)

## Only run examples in interactive R sessions
suppressPackageStartupMessages({
  library(shiny)
  library("plotly")
  library(lubridate)
  require(rgl)
  require(akima)
  library(dplyr)
  library(DT)
})

ui <- navbarPage("comtrade plotter", id = "tabs",
                 
                 # Sidebar with a slider input for number of bins 
                 tabPanel("Home",
                          fluidRow(
                            column(10,
                                   textInput("commodity_code",
                                             "Commodity:",
                                             value = NULL,
                                             placeholder = "Enter commodity code (HS code)"),
                                   textInput("reporter",
                                             "Reporter:",
                                             placeholder = "Enter reporter country 3-letter code"),
                                   textInput("partner",
                                             "Partner:",
                                             placeholder = "Enter partner country 3-letter code"),
                                   numericInput("start_date",
                                                "From:",
                                                min = 2013,
                                                max = as.integer(format(Sys.Date(), "%Y"))-1,
                                                value = as.integer(format(Sys.Date(), "%Y"))-1,
                                                step = 1),
                                   numericInput("end_date",
                                                "To:",
                                                min = 2013,
                                                max = as.integer(format(Sys.Date(), "%Y"))-1,
                                                value = as.integer(format(Sys.Date(), "%Y"))-1,
                                                step = 1),
                                   selectInput("plot_type",
                                               "Plot type:",
                                               choices = c("line" = "line",
                                                           "area" = "area")),
                                   actionButton("search", "Plot trade", class = "btn-info"))
                            )
                 )
)



server <- function(input, output, session) {
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)