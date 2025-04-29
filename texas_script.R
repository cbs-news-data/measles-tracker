# Libraries
library(rvest)
library(dplyr)
library(DatawRappr)
library(dotenv)
library(lubridate)
library(httr)
library(tigris)
library(readr)
library(sf)

options(tigris_use_cache = TRUE)
tx_counties <- counties(state = "TX", year = 2022)

options(tigris_class = "sf")  # make sure tigris returns sf objects

tx_counties <- counties(state = "TX", year = 2022)

# Calculate centroids and extract coordinates
tx_coords <- tx_counties %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(NAME, lat, lon) %>%
  rename(County = NAME)

table <- read_csv("data/texas_0404.csv")

total_cases <- sum(table$Cases)

# Get current date and time in UTC
current_datetime_utc <- Sys.time()
current_datetime_posix <- as.POSIXct(current_datetime_utc, tz = "UTC")


# Convert UTC to Eastern Time
current_datetime_eastern <- with_tz(current_datetime_utc, "America/New_York")

# Round the datetime to the nearest hour
rounded_datetime <- round_date(current_datetime_eastern, "hour")

# Format the date and time
formatted_datetime <- format(rounded_datetime, "%B %e, %Y at %l %p EDT.")


# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")

# Make the Datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(table, "lJkEW", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "lJkEW",
  api_key = dw_api_key,
  title = "Measles cases in Texas counties",
  intro = paste(
    "<strong>",
    total_cases,
    "</strong> measles cases have been reported in Texas this year. Click or hover over a county for more details."
  ),
  annotate = paste(
    "Last updated",
    formatted_datetime,
    "<br>Note: Texas Department of State Health Services updates data every Tuesday and Friday."
  ),
  byline = "Taylor Johnston / CBS News",
  source_name = "Texas Department of State Health Services",
  source_url = "https://www.dshs.texas.gov/news-alerts/measles-outbreak-feb-28-2025",
  folderId = "299930"
)

# Publish the chart
dw_publish_chart(chart_id = "lJkEW")
