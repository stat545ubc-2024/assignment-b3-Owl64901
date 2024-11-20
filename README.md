[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/_WsouPuM)

# U.S. Job Postings Information Shiny App

## Link to the App

Access the running instance of the Shiny app here: [shiny-app-job-postings](https://owl64901.shinyapps.io/shiny-app-job-postings/)

**Note: The app may take some time to load. Thank you for your patience.**

## Description

This Shiny app provides an interactive exploration of job postings across the United States. Users can filter job postings based on salary range, experience level, and state. The app displays the filtered data in an interactive table and visualizes the distribution of job postings on a U.S. map.

## Features

1. **Interactive Data Table using DT Package**  
   *The app uses the DT package to render the filtered job postings in an interactive table. This allows users to sort, search, and interact with the data easily, enhancing the data exploration experience.*

2. **Display Number of Results Found Whenever Filters Change**  
   *The app dynamically displays the number of job postings found based on the selected filters. This immediate feedback helps users understand the impact of their filter choices and adjust them as needed.*

3. **Download Filtered Data as a CSV File**  
   *Users can download the filtered job postings data as a CSV file directly from the app. This feature enables users to save the data for offline analysis or record-keeping.*

4. **Tab Layout with Data Table and Map using `tabsetPanel`**  
   *The app organizes content into two tabs using `tabsetPanel`: one for the data table and one for the map visualization. This layout enhances user navigation by separating different types of content into manageable sections.*

## Dataset

The dataset used in this app is the **LinkedIn Job Postings (2023 - 2024)** dataset from Kaggle. It contains a comprehensive collection of job postings scraped from LinkedIn, providing up-to-date information on job opportunities across various states and experience levels.

- **Dataset Source**: [LinkedIn Job Postings (2023 - 2024)](https://www.kaggle.com/datasets/arshkon/linkedin-job-postings)

