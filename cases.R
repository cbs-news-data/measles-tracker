# Libraries
library(dplyr)
library(janitor)
library(tidyr)
library(DatawRappr)
library(lubridate)
library(dotenv)
library(jsonlite)
library(readr)

# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")

# Load data
cases_by_year <- read_csv("data/cases.csv") %>% 
  clean_names() %>%
  mutate(year = as.character(year), cases = as.numeric(cases))  # Ensure correct types

# Fetch and clean new data from CDC
measles_by_year <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesYear.json") %>% 
  filter(filter == "2000-Present*") %>% 
  select(year, cases) %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%
  filter(year %in% c("2024", "2025")) %>%  # Proper filtering
  mutate(year = as.character(year), cases = as.numeric(cases))  # Ensure correct types

# Append new data and handle duplicate years (keep latest value)
cases_by_year <- cases_by_year %>%
  filter(year != "2024") %>%  # Remove outdated 2024 row
  bind_rows(measles_by_year) %>%  # Append new data
  arrange(year)  # Ensure chronological order

# Print updated dataframe
print(cases_by_year)

# Get current date and time in UTC
current_datetime_utc <- Sys.time()

# Convert UTC to Eastern Time
current_datetime_eastern <- with_tz(current_datetime_utc, "America/New_York")

# Round the datetime to the nearest hour
rounded_datetime <- round_date(current_datetime_eastern, "hour")

# Format the date and time
formatted_datetime <- format(rounded_datetime, "%B %e, %Y at %l %p EST.")

# Update the chart
measles_by_year <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesYear.json") %>% filter(filter == "2000-Present*") %>% select(year, cases)%>% 
  as.data.frame() %>% 
  janitor::clean_names()

dw_data_to_chart(cases_by_year, "Xf256", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "Xf256",
  api_key = dw_api_key,
  annotate = paste(
    "Last updated", formatted_datetime, "<br>Note: Current year case counts are preliminary."),
  folderId = "299930"
)

# Publish the chart
dw_publish_chart(chart_id = "Xf256")