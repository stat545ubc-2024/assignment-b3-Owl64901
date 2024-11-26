library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(DT)
library(maps)
library(stringr)
library(RColorBrewer) # Added for color palettes
library(colourpicker)  # Added for colourInput

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
  # Feature 7: Added custom CSS for enhanced visual appearance.
  # This links the 'styles.css' file located in the 'www' directory.
  includeCSS("www/styles.css"),
  
  # Feature 8: Add a LinkedIn Logo image to the UI.
  tags$div(
    class = "title",
    div(
      style = "display: flex; justify-content: center; align-items: center;",
      h1("U.S. Job Postings Information"),
      img(src = "LI-Logo.png", class = "logo", style = "margin-left: 10px;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      tags$strong("Filters"), # Title for the Filters box
      wellPanel(
        # State Code Filter
        tags$strong("State Code"),
        pickerInput(
          "state_dropdown", NULL,
          choices = sort(unique(df$state_code)), 
          selected = unique(df$state_code),  # Default to all states selected
          multiple = TRUE, 
          options = list(`actions-box` = TRUE)
        ),
        
        # Minimum Salary Filter
        tags$strong("Minimum Salary"),
        sliderInput(
          "min_salary_slider", NULL,
          min = 0, max = 150000, value = 30000, step = 1000  # Default min salary to 30000  
        ),
        
        # Maximum Salary Filter
        tags$strong("Maximum Salary"),
        sliderInput(
          "max_salary_slider", NULL,
          min = 0, max = 150000, value = 100000, step = 1000  # Default max salary to 100000
        ),
        
        # Experience Level Filter
        tags$strong("Experience Level"),
        checkboxGroupInput(
          "experience_level_checklist", NULL,
          choices = c("Entry level", "Mid-Senior level"), 
          selected = "Entry level"
        )
      ),
      
      br(), # Line break for spacing
      
      # Feature 6: Added colourInput controls to allow users to customize the colors of each region in the pie chart.
      tags$strong("Customize Pie Chart Colors (For the Second Tab)"), # Bold and appended text
      wellPanel(
        fluidRow(
          column(6,
                 colourpicker::colourInput("color_northeast", "Northeast", value = "#FBB4AE"),
                 colourpicker::colourInput("color_southeast", "Southeast", value = "#B3CDE3")
          ),
          column(6,
                 colourpicker::colourInput("color_midwest", "Midwest", value = "#CCEBC5"),
                 colourpicker::colourInput("color_southwest", "Southwest", value = "#DECBE4"),
                 colourpicker::colourInput("color_west", "West", value = "#FED9A6")
          )
        )
      )
    ),
    
    mainPanel(
      # Feature 4: Organize Data Table, Job Distribution, and Map into separate tabs using tabsetPanel.
      tabsetPanel(
        tabPanel("Data Table",
                 # Feature 3: Allow the user to download the table as a .csv file.
                 fluidRow(
                   column(12,
                          div(
                            p("Download the filtered data as a CSV file:", class = "download-description"),
                            downloadButton("downloadData", "Download CSV", class = "btn-download"),
                            style = "display: flex; justify-content: flex-end; align-items: center;"
                          )
                   )
                 ),
                 hr(),
                 # Feature 2: Displaying the number of results found whenever the filters change.
                 div(
                   textOutput("job_count_text"),
                   style = "font-weight: bold; font-size: 24px; text-align: center;"
                 ),
                 hr(),
                 # Feature 1: Render the interactive data table using DT with enhanced options.
                 DT::dataTableOutput("filtered_data_table")
        ),
        tabPanel("Job Distribution",
                 # Feature 5: Added a pie chart in a separate tab to show the percentage of job postings in each region.
                 plotOutput("region_pie_chart"),
                 p("Percentage of Job Postings by Region", align = "center", class = "plot-caption", style = "font-size: 18px;")
        ),
        tabPanel("Map",
                 plotlyOutput("jobs_map", width = "100%", height = "600px"),
                 p("Number of Job Postings by State", align = "center", class = "plot-caption", style = "font-size: 18px;")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    df %>%
      filter(
        min_salary >= input$min_salary_slider,
        max_salary <= input$max_salary_slider,
        formatted_experience_level %in% input$experience_level_checklist,
        state_code %in% input$state_dropdown
      )
  })
  
  # Feature 2: Displaying the number of results found whenever the filters change.
  output$job_count_text <- renderText({
    num_jobs <- nrow(filtered_data())
    paste("We found", num_jobs, "job postings for you.")
  })
  
  # Render the choropleth map
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
        colorscale = 'YlGnBu',
        marker = list(line = list(color = 'rgb(255,255,255)', width = 2))
      ) %>%
      layout(
        geo = list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          lakecolor = 'rgb(255, 255, 255)'
        )
      )
    
    fig
  })
  
  # Feature 1: Render the interactive data table using DT with enhanced options.
  output$filtered_data_table <- DT::renderDataTable({
    # Exclude the 'remote_allowed' column from the table
    table_data <- filtered_data() %>% select(-any_of("remote_allowed"))
    
    DT::datatable(
      table_data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        responsive = TRUE,
        scrollX = FALSE,
        columnDefs = list(
          list(targets = "_all", className = 'dt-center') # Center align all columns
        )
      ),
      rownames = FALSE,
      class = 'display nowrap'
    )
  })
  
  # Feature 3: Allow the user to download the table as a .csv file.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Exclude 'remote_allowed' if it exists
      data_to_download <- filtered_data() %>% select(-any_of("remote_allowed"))
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  # Feature 5: Added a pie chart in a separate tab to show the percentage of job postings in each region.
  output$region_pie_chart <- renderPlot({
    # Aggregate data to compute counts per region
    region_counts <- filtered_data() %>%
      group_by(region) %>%
      summarize(count = n()) %>%
      ungroup()
    
    # Check if there is data to plot
    if(nrow(region_counts) == 0){
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters.", size = 6, hjust = 0.5) +
        theme_void()
    } else {
      # Calculate percentages
      region_counts <- region_counts %>%
        mutate(percentage = round((count / sum(count)) * 100, 1))
      
      # Create the pie chart with percentage labels
      ggplot(region_counts, aes(x = "", y = percentage, fill = region)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        labs(
          fill = "Region"
        ) +
        theme_void() +
        theme(
          legend.title = element_text(size=12),
          legend.text = element_text(size=10)
        ) +
        scale_fill_manual(values = c(
          "Northeast" = input$color_northeast,
          "Southeast" = input$color_southeast,
          "Midwest"   = input$color_midwest,
          "Southwest" = input$color_southwest,
          "West"      = input$color_west
        )) + # Feature 6: Use user-selected colors
        geom_text(aes(label = paste0(percentage, "%")),
                  position = position_stack(vjust = 0.5),
                  size = 4,
                  color = "black")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)