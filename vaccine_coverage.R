# Libraries
library(dplyr)
library(janitor)
library(DatawRappr)
library(dotenv)
library(tidyverse)

# Load data
coverage <- read.csv("data/coverage.csv") %>% select (geography, school_year, estimate_pct) %>% pivot_wider(names_from = geography, values_from = estimate_pct) %>%
  filter(as.numeric(sub("-.*", "", school_year)) >= 2015) %>%
  arrange(school_year)  # Sort so oldest year is first


# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")

# Make the Datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(coverage, "IwPFs", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "IwPFs",
  api_key = dw_api_key,
  title = "Kindergarten measles vaccination rate by state",
  byline = "Taylor Johnston / CBS News",
  source_name = "CDC",
  source_url = "https://www.cdc.gov/measles/data-research/index.html",
  folderId = "299930"
)

# Publish the chart
dw_publish_chart(chart_id = "IwPFs")