# Libraries
library(dplyr)
library(rvest)

# URL of the webpage
url <- "https://www.cdc.gov/measles/data-research/index.html"  # Example, update with the actual URL

# Read the webpage
page <- read_html(url)

# Extract total cases
total_cases <- page %>%
  html_element("td.us-cases.left-border h4") %>%
  html_text() %>%
  as.numeric()

# Libraries
library(dplyr)
library(rvest)
library(DatawRappr)
library(lubridate)
library(dotenv)
library(lubridate)

# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")

# URL of the webpage
url <- "https://www.cdc.gov/measles/data-research/index.html"  # Example, update with the actual URL

# Read the webpage
page <- read_html(url)

# Extract total cases
total_cases <- page %>%
  html_element("td.us-cases.left-border h4") %>%
  html_text() %>%
  as.numeric()
