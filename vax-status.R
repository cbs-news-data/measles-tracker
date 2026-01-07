## cases not high enough yet to use


# Libraries
library(dplyr)
library(rvest)
library(stringr)
library(lubridate)
library(dotenv)
library(DatawRappr)
library(jsonlite)
# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")


# Read JSON data for total cases by year to get 2025 total cases
MeaslesCasesHospWeekly2025 <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesHospWeekly2025.json")
total_cases <- as.numeric(MeaslesCasesHospWeekly2025$Total_Cases)
unvax <- MeaslesCasesHospWeekly2025$Unvaccinated_or_Unknown
one_mmr <- MeaslesCasesHospWeekly2025$One_MMR_Dose
two_mmr <- MeaslesCasesHospWeekly2025$Two_MMR_Doses

# Remove the '%' symbol and convert to numeric (this removes everything except the numeric part)
unvax <- as.numeric(str_extract(unvax, "\\d+"))
one_mmr <- as.numeric(str_extract(one_mmr, "\\d+"))
two_mmr <- as.numeric(str_extract(two_mmr, "\\d+"))

# # URL of the webpage (replace with the actual URL)
# url <- "https://www.cdc.gov/measles/data-research/index.html"  # Example, update with the actual URL
# 
# # Read the webpage
# page <- read_html(url)
# 
# 
# # Extract total cases
# total_cases <- page %>%
#   html_element("td.us-cases.left-border h4") %>%
#   html_text() %>%
#   as.numeric()

# Print the total cases to verify
print(total_cases)
print(unvax)
print(one_mmr)
print(two_mmr)

# # Extract vaccination status text (targeting the entire 'td' that contains vaccination info)
# vax_text <- page %>%
#   html_elements("td.us-cases.left-border") %>%
#   html_text()
# 
# # Extract the relevant percentages from the text using regular expressions
# unvax <- str_extract(vax_text[3], "Unvaccinated or Unknown: (\\d+)%")
# one_mmr <- str_extract(vax_text[3], "One MMR dose: (\\d+)%")
# two_mmr <- str_extract(vax_text[3], "Two MMR doses: (\\d+)%")
# 
# # Remove the '%' symbol and convert to numeric (this removes everything except the numeric part)
# unvax <- as.numeric(str_extract(unvax, "\\d+"))
# one_mmr <- as.numeric(str_extract(one_mmr, "\\d+"))
# two_mmr <- as.numeric(str_extract(two_mmr, "\\d+"))
# 
# # Check the conversion to numeric
# print(unvax)
# print(one_mmr)
# print(two_mmr)


# Calculate the "At least one dose" percentage (sum of One MMR dose and Two MMR doses)
at_least_one_dose <- one_mmr + two_mmr

# Create a dataframe to store the vaccination status data
vax_df <- data.frame(
  Category = c("Unvaccinated or Unknown", "At least one dose"),
  Percentage = c(unvax, at_least_one_dose)
)


current_datetime_utc <- Sys.time()

# Convert UTC to Eastern Time
current_datetime_eastern <- with_tz(current_datetime_utc, "America/New_York")

# Round the datetime to the nearest hour
rounded_datetime <- round_date(current_datetime_eastern, "hour")

# Format the date and time
formatted_datetime <- format(rounded_datetime, "%B %e, %Y at %l %p EDT.")

dw_data_to_chart(vax_df, "qOFbx", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "qOFbx",
  api_key = dw_api_key,
  title = "Vaccination status among current cases",
  intro = paste0(
    "Of the <b>",
    prettyNum(total_cases, big.mark = ",", scientific = FALSE),
    "</b> cases in the U.S., ",
    "<b><span style='color: #1f5dc0;'>",
    unvax,
    "%</span></b> are either unvaccinated or have an unknown vaccination status, while <b><span style='color: #858585;'>",
    at_least_one_dose,
    "%</span></b> have received at least one dose of the MMR vaccine."
  ),
  annotate = paste(
    "Last updated", formatted_datetime,
    "<br>Note: Numbers may not add to 100% due to rounding."
  ),
  byline = "Taylor Johnston / CBS News",
  source_name = "CDC",
  source_url = "https://www.cdc.gov/measles/data-research/index.html",
  folderId = "299930"
)

# Publish the chart
dw_publish_chart(chart_id = "qOFbx")
