# Libraries
library(dplyr)
library(janitor)
library(DatawRappr)
library(dotenv)
library(tidyverse)

# Load data
coverage <- read.csv("data/coverage.csv") %>%
  select(school_year, geography, estimate_pct) %>%
  filter(as.numeric(sub("-.*", "", school_year)) >= 2015) %>%
  arrange(school_year) %>% filter(school_year == "2024-25")

coverage <- coverage %>%
  mutate(
    state_abbrev = state.abb[match(geography, state.name)]
  )


# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")

# Make the Datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(coverage, "EE6uW", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "EE6uW",
  api_key = dw_api_key,
  title = "Kindergarten measles vaccination rate by state",
  byline = "Taylor Johnston / CBS News",
  source_name = "CDC",
  source_url = "https://www.cdc.gov/measles/data-research/index.html",
  folderId = "376016"
)

# Publish the chart
dw_publish_chart(chart_id = "EE6uW")