library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(DT)
library(maps)
library(stringr)
library(RColorBrewer) # Added for color palettes
library(colourpicker)  # Feature 11: Added for colourInput

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
  # Feature 6: Include custom CSS to enhance the app's appearance.
  # This links the 'styles.css' file located in the 'www' directory.
  includeCSS("www/styles.css"),
  
  # Feature 10: Make the title bold and centered.
  tags$div(
    class = "title",
    titlePanel("U.S. Job Postings Information")
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Feature 5 & 13: Group filters into a single "Filters" box with bolded titles
      tags$strong("Filters"), # Title for the Filters box
      wellPanel(
        # Filter: State Code
        tags$strong("State Code"),
        pickerInput(
          "state_dropdown", NULL, # Removed description "Select States"
          choices = sort(unique(df$state_code)), 
          selected = unique(df$state_code),  # Default to all states selected
          multiple = TRUE, 
          options = list(`actions-box` = TRUE)
        ),
        
        # Filter: Minimum Salary
        tags$strong("Minimum Salary"),
        sliderInput(
          "min_salary_slider", NULL, # Removed description "Select Minimum Salary"
          min = 0, max = 150000, value = 30000, step = 1000  # Default min salary to 30000  
        ),
        
        # Filter: Maximum Salary
        tags$strong("Maximum Salary"),
        sliderInput(
          "max_salary_slider", NULL, # Removed description "Select Maximum Salary"
          min = 0, max = 150000, value = 100000, step = 1000  # Default max salary to 100000
        ),
        
        # Filter: Experience Level
        tags$strong("Experience Level"),
        checkboxGroupInput(
          "experience_level_checklist", NULL, # Removed description "Select Experience Levels"
          choices = c("Entry level", "Mid-Senior level"), 
          selected = "Entry level"
        )
      ),
      
      br(), # Line break for spacing
      
      # Feature 12 & 13: Make "Customize Pie Chart Colors" section larger and position it at the bottom
      tags$strong("Customize Pie Chart Colors (for second tab)"), # Bold and appended text
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
      # Feature 4 & 13: Organized tabs with reordered "Job Distribution" as the second tab
      tabsetPanel(
        tabPanel("Data Table",
                 # Feature 13: Add description and move download button above the table
                 fluidRow(
                   column(12,
                          div(
                            # Description and download button aligned to the right
                            div(
                              p("Download the filtered data as a CSV file:", class = "download-description"),
                              downloadButton("downloadData", "Download CSV", class = "btn-download"),
                              style = "display: inline-block; margin-left: 10px;"
                            ),
                            style = "text-align: right;"
                          )
                   )
                 ),
                 hr(),
                 div(
                   textOutput("job_count_text"),
                   style = "font-weight: bold; font-size: 24px; text-align: center;"
                 ),
                 hr(),
                 # Feature 1 & 6: Use the DT package to turn a static table into an interactive table.
                 # Make the table responsive and fit within the screen.
                 DT::dataTableOutput("filtered_data_table")
        ),
        tabPanel("Job Distribution",
                 # Feature 7 & 8: Pie Chart in a separate tab showing the percentage of job postings in each region.
                 # Feature 2: Removed the title from the pie chart and added a description below
                 plotOutput("region_pie_chart"),
                 # Feature 9 & 1: Add a caption to the pie chart with larger font
                 p("Percentage of Job Postings by Region", align = "center", class = "plot-caption", style = "font-size: 18px;")
        ),
        tabPanel("Map",
                 # Feature 3 & 13: Remove the title from the map and add a description below
                 plotlyOutput("jobs_map", width = "100%", height = "600px"),
                 # Feature 13: Add description under the map with larger font
                 p("Number of Job Postings by State", align = "center", class = "plot-caption", style = "font-size: 18px;")
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
        avg_salary >= input$min_salary_slider,
        avg_salary <= input$max_salary_slider,
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
        colorscale = 'YlGnBu', # Changed to a lighter color set
        marker = list(line = list(color = 'rgb(255,255,255)', width = 2))
      ) %>%
      layout(
        # Removed the title from the map as per Feature 13
        geo = list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          lakecolor = 'rgb(255, 255, 255)'
        )
      )
    
    fig
  })
  
  # Feature 1 & 6: Render the interactive data table using DT with enhanced options
  output$filtered_data_table <- DT::renderDataTable({
    # Exclude the 'remote_allowed' column from the table
    table_data <- filtered_data() %>% select(-remote_allowed)
    
    DT::datatable(
      table_data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        responsive = TRUE,
        scrollX = FALSE, # Disable horizontal scrolling
        columnDefs = list(
          list(targets = "_all", className = 'dt-center') # Center align all columns
        )
      ),
      rownames = FALSE,
      class = 'display nowrap' # Prevent wrapping of text
    )
  })
  
  # Feature 3: Provide download handler for downloading filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Exclude 'remote_allowed' before downloading
      write.csv(filtered_data() %>% select(-remote_allowed), file, row.names = FALSE)
    }
  )
  
  # Feature 7 & 8: Render the Pie Chart showing percentage of job postings by region with percentage labels
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
          # Removed the title from the pie chart as per Feature 2
          fill = "Region"
        ) +
        theme_void() +
        theme(
          # Removed plot.title styling as title is removed
          legend.title = element_text(size=12),
          legend.text = element_text(size=10)
        ) +
        scale_fill_manual(values = c(
          "Northeast" = input$color_northeast,
          "Southeast" = input$color_southeast,
          "Midwest"   = input$color_midwest,
          "Southwest" = input$color_southwest,
          "West"      = input$color_west
        )) + # Feature 11: Use user-selected colors
        # Feature 8: Add percentage labels to each sector
        geom_text(aes(label = paste0(percentage, "%")),
                  position = position_stack(vjust = 0.5),
                  size = 4,
                  color = "black")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)