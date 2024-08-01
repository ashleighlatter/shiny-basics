library(shiny)
library(bs4Dash)
library(fresh)
library(dplyr)
library(shinyWidgets)
library(shinyTime)
library(shinyDatetimePickers)

# App colours - override defaults
theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    olive = "#4A9094",
    purple = "#8965CD"
  ),
  bs4dash_status(
    primary = "#E1EDED",
    info = "#E4E4E4"
  )
)

ui <- dashboardPage(
  title = "Shiny Inputs",
  help = NULL,
  freshTheme = theme,
  
  # Header ----
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(title = "Shiny Inputs")
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    disable = TRUE
  ),
  
  # Body ----
  body = dashboardBody(
    
    fluidRow(
      
      # Standard date widgets
      box(
        title = "Date Inputs",
        width = 4,
        
        dateInput(
          inputId = "id_dateInput",
          label = "Select a date",
          value = "2024-07-22", # set default selected value
          weekstart = 1 # start week on monday instead of sunday
        ),
        
        dateRangeInput(
          inputId = "id_dateRangeInput",
          label = "Select a date range",
          start = "2024-01-01", # default start selected value
          end = "2024-06-30", # default end selected value
          weekstart = 1, # start week on monday
          min = "2023-01-01", # earliest selectable date
          max = "2025-12-31" # latest selectable date
        )
      ),
      
      # Print out selected date values
      box(
        title = "Selected values",
        width = 4,
        
        tags$b("dateInput:"),
        verbatimTextOutput("dateInput_val"),
        
        tags$b("dateRangeInput start:"),
        verbatimTextOutput("dateRangeInput_start"),
        
        tags$b("dateRangeInput end:"),
        verbatimTextOutput("dateRangeInput_end")
      ),
      
      box(
        title = "Update values",
        width = 4,
        
        # Used to update the values selected in dateInput and dateRangeInput
        selectInput(
          inputId = "new_value",
          label = "Select a date",
          choices = c("2024-01-01", "2023-05-29", "2021-12-29")
        )
      )
    ),
    
    # Alternative date/time widgets
    fluidRow(
      
      box(
        title = "airDatePicker",
        width = 4,
        airDatepickerInput(
          inputId = "id_airDatePicker",
          label = "Select a date and time",
          timepicker = TRUE
        )
      ),
      
      box(
        title = "shinyTime",
        width = 4,
        timeInput(
          inputId = "id_timeInput",
          label = "Select a time"
        )
      ),
      
      box(
        title = "shinyDatetimePickers",
        width = 4,
        datetimePickerInput(
          inputId = "id_datetimePickerInput"
        ),
        
        br(),
        datetimeMaterialPickerInput(
          inputId = "id_datetimePickerInput2"
        )
      )
    )
    
  )
)

server <- function(input, output) {
  
  # Access selected values for printing
  output$dateInput_val <- renderPrint({input$id_dateInput})
  output$dateRangeInput_start <- renderPrint({input$id_dateRangeInput[1]})
  output$dateRangeInput_end <- renderPrint({input$id_dateRangeInput[2]})
  
  # Update selected dateInput and dateRange input values when selectInput is changed
  observe({
    
    updateDateInput(
      inputId = "id_dateInput",
      value = input$new_value
    )
    
    updateDateRangeInput(
      inputId = "id_dateRangeInput",
      start = input$new_value
    )
    
  }) %>% 
    bindEvent(input$new_value)
  
}

shinyApp(ui, server)
