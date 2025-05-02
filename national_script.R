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
library(xml2)
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

# URL of the webpage (replace with the actual URL)
url <- "https://www.cdc.gov/measles/data-research/index.html"  # Example, update with the actual URL

# Read the webpage
page <- read_html(url)

# Extract total cases
total_cases <- page %>%
  html_element("td.us-cases.left-border h4") %>%
  html_text() %>%
  as.numeric()

# Print the total cases to verify
print(total_cases)


# Merge with state abbreviations (keeping all states)
measles_data <- state_abbreviations %>%
  right_join(measles_data, by = "state")

# Convert column to numeric and replace 0 with NA, then replace NA with an empty string
#measles_data <- measles_data %>%
 # mutate(cases_calendar_year = as.numeric(cases_calendar_year),
         #cases_calendar_year = na_if(cases_calendar_year, 0),
         #cases_calendar_year = ifelse(is.na(cases_calendar_year), "", cases_calendar_year))

# Combine NYC and New York State if both exist
#measles_data <- measles_data %>%
  #mutate(state = case_when(
    #state %in% c("New York City", "New York") ~ "New York",
    #TRUE ~ state
  #)) %>%
  #group_by(state) %>%
  #summarise(year = sum(as.numeric(cases_calendar_year), na.rm = TRUE), .groups = "drop")

# Get total number of confirmed cases (excluding blanks)
#total_cases <- sum(as.numeric(measles_data$cases_calendar_year), na.rm = TRUE)
#total_cases <- format(total_cases, big.mark = ",")

# Count number of states with reported cases
#count_non_na <- sum(measles_data$cases_calendar_year != "")

#list(total_cases = total_cases, count_non_na = count_non_na)
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
  intro = paste("So far this year, the U.S. has reported <b>", total_cases, "</b> cases. Click or hover over a state for more details."),
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
    "Last updated", formatted_datetime, "<br>Note: CDC updates data every Friday. Current year case counts are preliminary."),
  byline = "Taylor Johnston / CBS News",
  source_name = "CDC",
  source_url = "https://www.cdc.gov/measles/data-research/index.html",
  folderId = "299930"
)

# Publish the chart
dw_publish_chart(chart_id = "jUEyd")

write.csv(measles_data, "output/measles_data_clean.csv")

# XML FOR TV
# Define necessary variables
xml_title <- "Reported measles cases, 2025"
print(xml_title)
xml_subtitle <- paste("So far this year:", total_cases, "confirmed cases")
xml_source <- "CDC"
xml_date <- paste0("As of ", formatted_datetime)
xml_type <- "map"  # Map chart type
xml_qualifier <- " "  # One-line note, if needed

# Define xml_legendLabel variable (provide a suitable value)
xml_legendLabel <- "Confirmed cases by state"  # Add this line to define the legend label

# Make sure data is cleaned and correctly formatted
measles_data_clean <- measles_data %>% rename(label = state, value = cases_calendar_year)

# Check the structure of WN_data_clean to verify column names and contents
print(str(measles_data_clean))

measles_data_clean$value <- as.numeric(measles_data_clean$value)

# Define bins and labels
bins <- 1  # Continuous gradient
# Calculate max from the data
xml_binsMax <- max(measles_data_clean$value, na.rm = TRUE)
# Create label string based on dynamic max
xml_binsLabels <- paste0("0|", xml_binsMax)

# Create a new root node for the XML
measles_map <- xml_new_root("chart")

# Add title, subtitle, and type to the chart
xml_add_child(measles_map, "title", xml_title)
xml_add_child(measles_map, "subtitle", xml_subtitle)
xml_add_child(measles_map, "legendLabel", xml_legendLabel)
xml_add_child(measles_map, "type", xml_type)

# Add bin metadata
xml_add_child(measles_map, "bins", as.character(bins))
xml_add_child(measles_map, "binsLabels", xml_binsLabels)
xml_add_child(measles_map, "binsMax", as.character(xml_binsMax))

# Find the top 3 values based on reported cases
top_3_values_gradient <- measles_data_clean %>%
  arrange(desc(value)) %>%
  head(3)

# Add data rows
for (i in 1:nrow(measles_data_clean)) {
  # Extract the label and value for each row
  label_value <- as.character(measles_data_clean$label[i])
  value_value <- as.character(measles_data_clean$value[i])
  
  # Create a new dataPoint node
  row_node <- xml_add_child(measles_map, "dataPoint")
  
  # Add label and value to the dataPoint node
  xml_add_child(row_node, "label", label_value)
  xml_add_child(row_node, "value", value_value)
  
  # Check if the current value is one of the top 3 values
  if (measles_data_clean$value[i] %in% top_3_values_gradient$value) {
    # Highlight the top 3 values (showValue = 1)
    show_value <- "1"
    show_label <- "1"
  } else {
    # For all other values, set showValue and showLabel to 0
    show_value <- "0"
    show_label <- "0"
  }
  
  # Add showValue and showLabel
  xml_add_child(row_node, "showValue", show_value)
  xml_add_child(row_node, "showLabel", show_label)
  
  # Add valueToShow if showValue is 1, otherwise leave it empty
  if (show_value == "1") {
    xml_add_child(row_node, "valueToShow", value_value)
  } else {
    xml_add_child(row_node, "valueToShow", "")
  }
}

# Add source, date, and qualifier to the root chart node
xml_add_child(measles_map, "source", xml_source)
xml_add_child(measles_map, "date", xml_date)
xml_add_child(measles_map, "qualifier", xml_qualifier)

# Write the XML to a file
write_xml(measles_map, "output/xml/gradient.xml")