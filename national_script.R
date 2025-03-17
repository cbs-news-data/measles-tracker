# Libraries
library(dplyr)
library(janitor)
library(tidyr)
library(DatawRappr)
library(lubridate)
library(dotenv)
library(xml2)
library(jsonlite)
library(readr)

# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")

# Load state abbreviations
state_abbreviations <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
            "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
            "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
            "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
            "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", 
                   "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", 
                   "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
                   "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)

# Read JSON data
measles_data <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesMap.json") %>% filter(year == "2025") %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%
  rename(state = geography)  # Rename geography column to state


# Merge with state abbreviations (keeping all states)
measles_data <- state_abbreviations %>%
  right_join(measles_data, by = "state")

# Convert column to numeric and replace 0 with NA, then replace NA with an empty string
measles_data <- measles_data %>%
  mutate(cases_calendar_year = as.numeric(cases_calendar_year),
         cases_calendar_year = na_if(cases_calendar_year, 0),
         cases_calendar_year = ifelse(is.na(cases_calendar_year), "", cases_calendar_year))

# Get total number of confirmed cases (excluding blanks)
total_cases <- sum(as.numeric(measles_data$cases_calendar_year), na.rm = TRUE)
total_cases <- format(total_cases, big.mark = ",")

# Count number of states with reported cases
count_non_na <- sum(measles_data$cases_calendar_year != "")

list(total_cases = total_cases, count_non_na = count_non_na)
# Get date for Datawrapper
# Get current date and time in UTC
current_datetime_utc <- Sys.time()

# Convert UTC to Eastern Time
current_datetime_eastern <- with_tz(current_datetime_utc, "America/New_York")

# Round the datetime to the nearest hour
rounded_datetime <- round_date(current_datetime_eastern, "hour")

# Format the date and time
formatted_datetime <- format(rounded_datetime, "%B %e, %Y at %l %p EST.")

# Make the Datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(measles_data, "IF2bI", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "IF2bI",
  api_key = dw_api_key,
  title = "Measles cases by state",
  intro = paste("So far this year, the U.S. has reported <b>", total_cases, "</b> cases. Click or hover over a state for more details."),
  annotate = paste(
    "Last updated", formatted_datetime, "<br>Note: CDC updates data every Friday. Case counts are preliminary."),
  byline = "Taylor Johnston / CBS News",
  source_name = "CDC",
  source_url = "https://www.cdc.gov/measles/data-research/index.html",
  folderId = "299930"
)

# Publish the chart
dw_publish_chart(chart_id = "IF2bI")


# BAR CHART
measles_by_year <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesYear.json") %>% filter(filter == "2000-Present*") %>% select(year, cases)%>% 
  as.data.frame() %>% 
  janitor::clean_names()

dw_data_to_chart(measles_by_year, "jUEyd", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "jUEyd",
  api_key = dw_api_key,
  title = "Measles cases by year",
  annotate = paste(
    "Last updated", formatted_datetime, "<br>Note: CDC updates data every Friday. Current year case counts are preliminary."),
  byline = "Taylor Johnston / CBS News",
  source_name = "CDC",
  source_url = "https://www.cdc.gov/measles/data-research/index.html",
  folderId = "299930"
)

# Publish the chart
dw_publish_chart(chart_id = "jUEyd")