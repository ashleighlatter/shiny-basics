library(shiny)
library(bs4Dash)
library(dplyr)
library(readr)
library(plotly)
library(DT)
library(fresh)

# App colours
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

# Load and tidy data
data_path <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bird-survey-results-for-areas-in-the-city-of-melbourne-february-and-march-2018/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C"

bird_sightings <- read_csv(data_path) %>% 
  # select and rename relevant columns
  select(
    Date = `Sighting Date`,
    Bird = `Common Name`,
    Sightings = `Sighting Count`,
    Location = site_name
  )

ui <- dashboardPage(
  title = "Bird Sightings",
  freshTheme = theme,
  dark = NULL,
  help = NULL,
  
  # Header ----
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(
      title = "Bird Sightings",
      color = "olive",
      image = "https://images.unsplash.com/photo-1539664030485-a936c7d29c6e?q=80&w=1160&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
    )
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    minified = FALSE,
    width = "350px",
    
    # Filters
    dateRangeInput(
      inputId = "selected_date",
      label = "Selected Date Range",
      start = min(bird_sightings$Date),
      end = max(bird_sightings$Date),
      width = "100%"
    ),
    
    selectInput(
      inputId = "selected_bird",
      label = "Selected Bird",
      choices = unique(bird_sightings$Bird),
      width = "100%"
    ),
    
    hr(),
    
    # Button to trigger update
    actionButton(
      inputId = "refresh_button",
      label = "Refresh",
      icon = icon("arrows-rotate")
    )
  ),
  
  # Body ----
  body = dashboardBody(
    
    fluidRow(
      box(
        title = "Bird Count by Location",
        width = 6,
        plotlyOutput("plot_birds_by_site")
      ),
      
      box(
        title = "Birds Sighted by Day",
        width = 6,
        plotlyOutput("plot_birds_by_day")
      )
    ),
    
    fluidRow(
      box(
        title = "Data",
        width = 12,
        DTOutput("table_data")
      )
    )
    
  )
)

server <- function(input, output) {
  
  # Reactive dataframe
  filtered_data <- reactive({
    
    bird_sightings %>% 
      filter(
        between(Date, input$selected_date[1], input$selected_date[2]),
        Bird == input$selected_bird
      )
    
  }) %>% 
    # tells reactive() to only update after a specified event
    bindEvent(input$refresh_button, ignoreNULL = FALSE)
    
  
  # Number of birds sighted per day
  output$plot_birds_by_day <- renderPlotly({
    
    plot_data <- filtered_data() %>% 
      group_by(Date) %>% 
      summarise(birds_sighted = sum(Sightings))
    
    plot_ly(
      data = plot_data,
      x = ~Date,
      y = ~birds_sighted,
      type = "scatter",
      mode = "lines",
      line = list(color = "#8965CD")
    ) %>% 
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Birds Sighted")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Number birds by site
  output$plot_birds_by_site <- renderPlotly({
    
    plot_data <- filtered_data() %>% 
      group_by(Location) %>% 
      summarise(bird_count = sum(Sightings)) %>% 
      arrange(bird_count) %>% 
      mutate(Location = if_else(Location == "Dynon Road Tidal Canal Wildlife Sanctuary", "Dynon Road Tidal Canal<br>Wildlife Sanctuary", Location)) %>% 
      mutate(Location = factor(Location, levels = .$Location))
    
    plot_ly(
      data = plot_data,
      x = ~bird_count,
      y = ~Location,
      type = "bar",
      marker = list(color = "#8965CD"),
      orientation = "h"
    ) %>% 
      layout(
        xaxis = list(title = "Birds Sighted"),
        yaxis = list(title = "") 
      ) %>%
      config(displayModeBar = FALSE)
    
  })
  
  # Data to display in table
  output$table_data <- renderDT({
    
    filtered_data() %>% 
      arrange(Location, desc(Date))
    
  })
  
}

shinyApp(ui, server)
