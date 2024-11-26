[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/_WsouPuM)

# U.S. Job Postings Information Shiny App

## Links to the App

- **New Version:** [shiny-app-job-postings-new-version](https://owl64901.shinyapps.io/shiny-app-job-postings-new-version/)
  
  *Note: The new version includes additional features and improvements.*

- **Old Version:** [shiny-app-job-postings](https://owl64901.shinyapps.io/shiny-app-job-postings/)
  
  *Note: The old version of the app is still accessible for reference.*

**Note:** Both versions may take some time to load. Thank you for your patience.

## Description

This Shiny app provides an interactive exploration of job postings across the United States. Users can filter job postings based on salary range, experience level, and state. The app displays the filtered data in an interactive table and visualizes the distribution of job postings on a U.S. map and through a pie chart.

## Features

1. **Interactive Data Table using DT Package**  
   *The app uses the DT package to render the filtered job postings in an interactive table. This allows users to sort, search, and interact with the data easily, enhancing the data exploration experience.*

2. **Display Number of Results Found Whenever Filters Change**  
   *The app dynamically displays the number of job postings found based on the selected filters. This immediate feedback helps users understand the impact of their filter choices and adjust them as needed.*

3. **Download Filtered Data as a CSV File**  
   *Users can download the filtered job postings data as a CSV file directly from the app. This feature enables users to save the data for offline analysis or record-keeping.*

4. **Organize Data Table, Job Distribution, and Map into Separate Tabs using `tabsetPanel`**  
   *The app organizes content into three tabs using `tabsetPanel`: one for the data table, one for the pie chart visualization, and one for the map visualization. This layout enhances user navigation by separating different types of content into manageable sections.*

5. **Interactive Pie Chart Visualization of Job Postings by Region**  
   *A separate tab displays an interactive pie chart showing the percentage of job postings in each U.S. region (Northeast, Southeast, Midwest, Southwest, West). This chart updates dynamically as users adjust the filters, providing a real-time visual representation of job distribution across different regions.*

6. **Customizable Pie Chart Colors with `colourInput` Controls**  
   *Users can customize the colors of each region in the pie chart using `colourInput` controls. This feature allows for personalized visualization preferences, enhancing the user experience.*

7. **Enhanced Visual Appearance with Custom CSS**  
   *The app incorporates custom CSS to improve its overall appearance. This includes styling for the title, sidebar, main panel, tabs, buttons, and other UI elements, ensuring a polished and professional look.*

8. **LinkedIn Logo Integration in the UI**  
   *A LinkedIn logo is added next to the app's title, providing a professional touch. The logo is appropriately sized and aligned for a cohesive design.*

## Usage

1. **Access the App:**
   - Click on the link to the **New Version** of the app: [shiny-app-job-postings-new-version](https://owl64901.shinyapps.io/shiny-app-job-postings-new-version/)

2. **Apply Filters:**
   - In the **Filters** sidebar on the left, select the states you are interested in from the **State Code** dropdown.
   - Adjust the **Minimum Salary** and **Maximum Salary** sliders to set your desired salary range.
   - Choose the **Experience Level** by selecting one or more options from the checkbox group.

3. **Customize Pie Chart Colors (Optional):**
   - Scroll down in the sidebar to the **Customize Pie Chart Colors** section.
   - Use the color pickers to select your preferred colors for each region.

4. **Navigate Through Tabs:**
   - **Data Table Tab:** View the filtered job postings in an interactive table. You can sort, search, and explore the data. Click the **Download CSV** button to export the data.
   - **Job Distribution Tab:** View an interactive pie chart showing the distribution of job postings by region. The chart updates dynamically based on your selected filters.
   - **Map Tab:** Explore a choropleth map displaying the number of job postings per state. The map also updates in real-time as you adjust the filters.

5. **Interact with Visualizations:**
   - Use the filters to refine the data and instantly see updates in the table and visualizations.

6. **Download Data:**
   - Click the **Download CSV** button in the **Data Table** tab to download the currently filtered job postings data for offline analysis.

## Dataset

The dataset used in this app is the **LinkedIn Job Postings (2023 - 2024)** dataset from Kaggle. It contains a comprehensive collection of job postings scraped from LinkedIn, providing up-to-date information on job opportunities across various states and experience levels.

- **Dataset Source**: [LinkedIn Job Postings (2023 - 2024)](https://www.kaggle.com/datasets/arshkon/linkedin-job-postings)
