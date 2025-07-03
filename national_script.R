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
library(rvest)

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

# Read JSON data for total cases by year to get 2025 total cases
total_cases_2025 <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesYear.json") %>% 
  filter(year == "2025") %>% 
  filter(filter == "2000-Present*")

total_cases <- as.numeric(total_cases_2025$cases)

# # URL of the webpage (replace with the actual URL)
# url <- "https://www.cdc.gov/measles/data-research/index.html"  # Example, update with the actual URL
# 
# # Read the webpage
# page <- read_html(url)
# 
# 
# # Extract total cases
# total_cases <- page %>%
#   html_node(xpath = '//*[@id="Total_Cases"]') %>%
#   html_text() #%>%
#   as.numeric()

# Print the total cases to verify
print(total_cases)

# Merge with state abbreviations (keeping all states)
measles_data <- state_abbreviations %>%
  right_join(measles_data, by = "state")

# Get date for Datawrapper
# Get current date and time in UTC
current_datetime_utc <- Sys.time()


# Convert UTC to Eastern Time
current_datetime_eastern <- with_tz(current_datetime_utc, "America/New_York")

# Round the datetime to the nearest hour
rounded_datetime <- round_date(current_datetime_eastern, "hour")

# Format the date and time
formatted_datetime <- format(rounded_datetime, "%B %e, %Y at %l %p EDT.")

# Make the Datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(measles_data, "IF2bI", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "IF2bI",
  api_key = dw_api_key,
  title = "Measles cases by state",
  intro = paste("So far this year, the U.S. has reported <b>", prettyNum(total_cases, big.mark = ",", scientific = FALSE), "</b> cases."),
  annotate = paste(
    "Last updated", formatted_datetime, "<br>Note: CDC updates data every Friday."),
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
    "Last updated", formatted_datetime, "<br>Note: CDC updates data every Wednesday. Current year case counts are preliminary."),
  byline = "Taylor Johnston / CBS News",
  source_name = "CDC",
  source_url = "https://www.cdc.gov/measles/data-research/index.html",
  folderId = "299930"
)

# Publish the chart
dw_publish_chart(chart_id = "jUEyd")

write.csv(measles_data, "output/measles_data_clean.csv")

# Write XML to file - BINNED

# Define necessary variables
xml_title <- "Reported measles cases, 2025"
print(xml_title)
xml_subtitle <- paste("So far this year:", total_cases, "confirmed cases")
xml_source <- "CDC"
xml_date <- paste0("As of ", formatted_datetime)
xml_type <- "map"  # Map chart type
xml_qualifier <- " "  # One-line note, if needed

# Define xml_legendLabel variable (provide a suitable value)
xml_legendLabel <- "Confirmed Cases by State"  # Add this line to define the legend label

# Make sure data is cleaned and correctly formatted
measles_data_binned <- measles_data %>% rename(value = cases_range)

# Check the structure of WN_data_clean to verify column names and contents
print(str(measles_data_binned))

# Add binned data to the dataframe
measles_data_binned <- measles_data

# Define bins and labels
# Define bins and labels
bins <- c(0, 1, 10, 50, 100, 250, Inf)
# Use this predefined order of bins
labels <- c("0", "1-9", "10-49", "50-99", "100-249", "250+")

# Generate pipe-separated labels for XML
xml_binsLabels <- paste(labels, collapse = "|")
xml_bins <- length(labels)
xml_binsMax <- 250  # Still true

# Use the data as-is
measles_data_clean_binned <- measles_data_binned %>%
  select(label = state, value = cases_range)

# For top 3, use a custom order
# We'll convert labels into numeric midpoints to rank them
label_midpoints <- c("0" = 0, "1-9" = 5, "10-49" = 30, "50-99" = 75, "100-249" = 175, "250+" = 300)

measles_data_clean_binned$numeric_value <- label_midpoints[measles_data_clean_binned$value]

top_3_values_binned <- measles_data_clean_binned %>%
  arrange(desc(numeric_value)) %>%
  head(3)

# Start XML
measles_map_binned <- xml_new_root("chart")
xml_add_child(measles_map_binned, "title", xml_title)
xml_add_child(measles_map_binned, "subtitle", xml_subtitle)
xml_add_child(measles_map_binned, "legendLabel", xml_legendLabel)
xml_add_child(measles_map_binned, "type", xml_type)
xml_add_child(measles_map_binned, "bins", as.character(xml_bins))
xml_add_child(measles_map_binned, "binsLabels", xml_binsLabels)
xml_add_child(measles_map_binned, "binsMax", as.character(xml_binsMax))

# Add data rows
for (i in 1:nrow(measles_data_clean_binned)) {
  row_node <- xml_add_child(measles_map_binned, "dataPoint")
  
  label_value <- as.character(measles_data_clean_binned$label[i])
  value_value <- as.character(measles_data_clean_binned$value[i])
  num_value <- measles_data_clean_binned$numeric_value[i]
  
  xml_add_child(row_node, "label", label_value)
  xml_add_child(row_node, "value", value_value)
  
  is_top <- num_value %in% top_3_values_binned$numeric_value
  
  xml_add_child(row_node, "showValue", ifelse(is_top, "1", "0"))
  xml_add_child(row_node, "showLabel", ifelse(is_top, "1", "0"))
  xml_add_child(row_node, "valueToShow", ifelse(is_top, value_value, ""))
}

# Footer info
xml_add_child(measles_map_binned, "source", xml_source)
xml_add_child(measles_map_binned, "date", xml_date)
xml_add_child(measles_map_binned, "qualifier", xml_qualifier)

# Save XML
write_xml(measles_map_binned, "output/xml/binned.xml")