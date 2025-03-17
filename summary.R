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

# Print
print(total_cases)

# Extract the relevant percentages from the text using regular expressions
unvax <- str_extract(vax_text[3], "Unvaccinated or Unknown: (\\d+)%")
one_mmr <- str_extract(vax_text[3], "One MMR dose: (\\d+)%")
two_mmr <- str_extract(vax_text[3], "Two MMR doses: (\\d+)%")

# Remove the '%' symbol and convert to numeric (this removes everything except the numeric part)
unvax <- as.numeric(str_extract(unvax, "\\d+"))
one_mmr <- as.numeric(str_extract(one_mmr, "\\d+"))
two_mmr <- as.numeric(str_extract(two_mmr, "\\d+"))

# Check the conversion to numeric
print(unvax)
print(one_mmr)
print(two_mmr)

# Calculate the "At least one dose" percentage (sum of One MMR dose and Two MMR doses)
at_least_one_dose <- one_mmr + two_mmr

# Create a dataframe to store the vaccination status data
vax_df <- data.frame(
  Category = c("Unvaccinated or Unknown", "At least one dose"),
  Percentage = c(unvax, at_least_one_dose)
)


# Print the dataframe
print(vax_df)

# Extract vaccination status text (targeting the entire 'td' that contains vaccination info)
vax_text <- page %>%
  html_elements("td.us-cases.left-border") %>%
  html_text()

