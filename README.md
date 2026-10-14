
Google Data Analytics Capstone: Cyclistic Bike-Share Case Study

This repository contains the complete R programming workflow for the Google Data Analytics Professional Certificate capstone project. The analysis focuses on a fictional bike-share company in Chicago, Cyclistic, with the objective of providing data-driven recommendations to convert casual riders into annual members.

Project Scenario & Objective

Cyclistic’s marketing director believes the company’s future success depends heavily on maximizing the number of annual memberships. As a junior data analyst, the task is to understand how casual riders and annual members use Cyclistic bikes differently.
This project strictly follows the 6 phases of the Google Data Analysis process:
1. Ask 2. Prepare 3. Process 4. Analyze 5. Share 6. Act

Technical Stack & Packages Used

Because of the massive size of the historical trip datasets, R and RStudio were selected to handle the end-to-end data processing, statistical computing, and visualization.
The script leverages the following libraries:
• tidyverse (incorporating dplyr, ggplot2, readr, tibble, purrr, stringr) – For data manipulation, exploration, and visualization.
• lubridate – For processing, formatting, and extracting date/time components.
• janitor – For data cleaning and verifying structural consistency.
• scales – For aesthetic data formating in final visualizations.

Data Analysis Lifecycle Workflow


1. Ask & Prepare

• Business Task: Identify behavioural differences between casual riders and annual members to form a strategy for target marketing campaigns.
• Data Source: Publicly available historical trip data provided by Cyclistic (Amazon S3 bucket container).

2. Process (Data Cleaning & Transformation)

• Consolidating 12 individual monthly CSV data tables into a unified master dataframe.
• Inspecting column structures, removing duplicate ride_id values, and scrubbing whitespace errors.
• Filtering out data entries showing negative trip lengths or testing entries created by system maintenance.
• Creating new derived attributes:
	• ride_length: Calculating the exact duration of each trip in seconds (ended_at minus started_at).
	• day_of_week: Extracting the day the trip took place (Sunday through Saturday).

3. Analyze & Share (Exploratory Analytics)

• Calculating descriptive statistics (Mean, Median, Max, and Min trip lengths grouped by rider type).
• Isolating patterns across:
	• Ride Frequency: Comparing the raw volume of rides between members and casuals across days of the week and seasons.
	• Duration: Analyzing how long each user segment keeps a bike checked out.
	• Bike Preference: Breaking down the use of classic, electric, or docked bikes across user segments.
• Visualizing insights using ggplot2 to track weekly distribution spikes and average travel durations.

4. Act

• Translating raw data metrics into high-level strategic takeaways for the Cyclistic executive team to deploy targeted marketing initiatives.

How to Run the Script Locally


1. Clone the Project

bash
git clone https://github.com
cd Google-data-Analytics-Case-study

2. Set Up the Data

1. Download the raw Cyclistic historical trip data (CSV files) recommended by the course manual.
2. Create a folder named data/ within your project root directory and add the CSV files there.

3. Execute the R Script

Open RStudio or your preferred R environment and run:
R
# Source the file to run the entire analytical flow
source("Case Study.R")
