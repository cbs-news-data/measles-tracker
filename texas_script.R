# Libraries
library(rvest)
library(dplyr)
library(DatawRappr)
library(dotenv)
library(lubridate)
library(httr)

url_exists <- function(url) {
  tryCatch({
    response <- GET(url)
    return(status_code(response) == 200)  # returns TRUE if status code is 200 (OK)
  }, error = function(e) {
    return(FALSE)  # returns FALSE if there's an error
  })
}

# Function to get the next Tuesday or Friday date
get_next_update_date <- function(current_date) {
  # Find next Tuesday
  next_tuesday <- floor_date(current_date, "week") + days(2)
  
  # Find next Friday
  next_friday <- floor_date(current_date, "week") + days(5)
  
  # Determine which one is closer: Tuesday or Friday
  if (current_date <= next_tuesday) {
    return(next_tuesday)
  } else {
    return(next_friday)
  }
}

# Get the current date
current_date <- Sys.Date()

# Get the next update date (either next Tuesday or Friday)
next_update_date <- get_next_update_date(current_date)

# Initial URL for February 28, 2025
initial_url <- "https://www.dshs.texas.gov/news-alerts/measles-outbreak-feb-28-2025"

# Format the next update URL
formatted_url <- paste0("https://www.dshs.texas.gov/news-alerts/measles-outbreak-", format(next_update_date, "%b-%d-%Y"))

# Print initial URL and next update URL for reference
print(paste("Initial URL:", initial_url))
print(paste("Next Update URL:", formatted_url))

# Determine which URL to use (check if next update URL exists)
url_to_use <- if (url_exists(formatted_url)) {
  formatted_url  # Use the next update URL if it exists
} else {
  initial_url  # Fall back to the initial URL if the next update URL doesn't exist
}

# Fetch the webpage content
webpage <- read_html(url_to_use)

# Extract the table data
table <- webpage %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE)

# View the table
print(table)

# Count number of states with reported cases
total_cases <- table %>% filter(County == "Total") %>% pull(Cases)

# Get current date and time in UTC
current_datetime_utc <- Sys.time()

# Convert UTC to Eastern Time
current_datetime_eastern <- with_tz(current_datetime_utc, "America/New_York")

# Round the datetime to the nearest hour
rounded_datetime <- round_date(current_datetime_eastern, "hour")

# Format the date and time
formatted_datetime <- format(rounded_datetime, "%B %e, %Y at %l %p EST.")


# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")

# Make the Datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(table, "O3V8f", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "O3V8f",
  api_key = dw_api_key,
  title = "Measles cases in Texas counties",
  intro = paste("<strong>", total_cases, "</strong> measles cases have been reported in Texas this year. Click or hover over a county for more details."),
  annotate = paste(
    "Last updated", formatted_datetime, "<br>Note: Texas Department of State Health Services updates data every Tuesday and Friday."),
  byline = "Taylor Johnston / CBS News",
  source_name = "Texas Department of State Health Services",
  source_url = "https://www.dshs.texas.gov/news-alerts/measles-outbreak-feb-28-2025",
  folderId = "299930"
)

# Publish the chart
dw_publish_chart(chart_id = "O3V8f")
