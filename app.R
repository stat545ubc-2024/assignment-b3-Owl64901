library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(DT)
library(maps)
library(stringr)

# Load and clean the dataset

# Load the dataset
df <- read.csv("postings.csv")

# Data cleaning
# Select relevant columns
selected_columns <- c(
  'title', 'max_salary', 'min_salary', 'pay_period', 'formatted_work_type', 
  'location', 'remote_allowed', 'formatted_experience_level'
)
df_selected <- df %>% select(all_of(selected_columns))

# Extract 'state_code' from 'location' field
df_selected <- df_selected %>%
  mutate(
    state_code = str_trim(sapply(strsplit(location, ","), function(x) tail(x, 1)))
  )

# Create mapping from state_code to region
regions <- list(
  Northeast = c('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NY', 'PA', 'NJ', 'DE', 'MD'),
  Southeast = c('FL', 'GA', 'NC', 'SC', 'VA', 'DC', 'WV', 'AL', 'KY', 'MS', 'TN', 'AR', 'LA'),
  Midwest   = c('IL', 'IN', 'MI', 'OH', 'WI', 'IA', 'KS', 'MN', 'MO', 'NE', 'ND', 'SD'),
  Southwest = c('AZ', 'NM', 'OK', 'TX'),
  West      = c('CO', 'ID', 'MT', 'NV', 'UT', 'WY', 'AK', 'CA', 'HI', 'OR', 'WA')
)

# Flatten the regions list to create a state to region mapping
state_to_region <- unlist(lapply(names(regions), function(region) {
  states <- regions[[region]]
  setNames(rep(region, length(states)), states)
}))

# Map 'state_code' to 'region'
df_selected$region <- state_to_region[df_selected$state_code]

# Filter out rows where 'region' is NA (i.e., states not in the mapping)
df_cleaned <- df_selected %>% filter(!is.na(region))

# Compute 'avg_salary'
df_cleaned <- df_cleaned %>%
  mutate(avg_salary = rowMeans(select(., min_salary, max_salary), na.rm = TRUE))

# Use the cleaned data in the app
df <- df_cleaned

# Get states map data
states_map <- map_data("state")

# Define UI
ui <- fluidPage(
  titlePanel("U.S. Job Postings Information"),
  
  sidebarLayout(
    sidebarPanel(
      # Filters at the top
      pickerInput(
        "state_dropdown", "State Code", 
        choices = sort(unique(df$state_code)), 
        selected = unique(df$state_code),  # Default to all states selected
        multiple = TRUE, 
        options = list(`actions-box` = TRUE)
      ),
      sliderInput(
        "min_salary_slider", "Minimum Salary", 
        min = 0, max = 150000, value = 30000, step = 1000  # Default min salary to 30000  
      ),
      sliderInput(
        "max_salary_slider", "Maximum Salary", 
        min = 0, max = 150000, value = 100000, step = 1000  # Default max salary to 100000
      ),
      checkboxGroupInput(
        "experience_level_checklist", "Experience Level", 
        choices = c("Entry level", "Mid-Senior level"), 
        selected = "Entry level"
      )
    ),
    
    mainPanel(
      # Feature 4: Placing table and map in two tabs using tabsetPanel
      # This organizes content into separate tabs, enhancing user navigation.
      tabsetPanel(
        tabPanel("Data Table",
                 # Feature 2: Displaying the number of results found whenever the filters change.
                 # Provides immediate feedback to users about the number of matching results.
                 # The text is bold, large, and centered for emphasis.
                 div(
                   textOutput("job_count_text"),
                   style = "font-weight: bold; font-size: 24px; text-align: center;"
                 ),
                 hr(),
                 # Feature 1: Use the DT package to turn a static table into an interactive table.
                 # This allows users to sort, search, and interact with the data.
                 DT::dataTableOutput("filtered_data_table"),
                 # Feature 3: Allow the user to download the table as a .csv file.
                 # This enables users to download data for further analysis or record-keeping.
                 downloadButton("downloadData", "Download Filtered Data")
        ),
        tabPanel("Map",
                 plotlyOutput("jobs_map", width = "100%", height = "600px")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression for filtered data based on user inputs
  filtered_data <- reactive({
    df %>%
      filter(
        min_salary >= input$min_salary_slider,
        max_salary <= input$max_salary_slider,
        formatted_experience_level %in% input$experience_level_checklist,
        state_code %in% input$state_dropdown
      )
  })
  
  # Feature 2: Compute and display the number of job postings found
  output$job_count_text <- renderText({
    num_jobs <- nrow(filtered_data())
    paste("We found", num_jobs, "job postings for you.")
  })
  
  output$jobs_map <- renderPlotly({
    filtered_df <- filtered_data() %>%
      group_by(state_code) %>%
      summarize(job_count = n()) %>%
      ungroup()
    
    if (nrow(filtered_df) == 0 || all(is.na(filtered_df$job_count))) {
      zmax <- 1
    } else {
      zmax <- max(filtered_df$job_count, na.rm = TRUE)
    }
    
    fig <- plot_geo(filtered_df, locationmode = 'USA-states') %>%
      add_trace(
        locations = ~state_code, 
        z = ~job_count, 
        text = ~paste(state_code, job_count, "jobs"), 
        zmin = 0,
        zmax = zmax,
        type = 'choropleth',
        marker = list(line = list(color = 'rgb(255,255,255)', width = 2))
      ) %>%
      layout(
        title = 'Number of Job Postings by State',
        geo = list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          lakecolor = 'rgb(255, 255, 255)'
        )
      )
    
    fig
  })
  
  # Feature 1: Render the interactive data table using DT
  output$filtered_data_table <- DT::renderDataTable({
    DT::datatable(filtered_data())
  })
  
  # Feature 3: Provide download handler for downloading filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)