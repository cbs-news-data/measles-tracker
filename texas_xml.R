library(tidyverse)
library(stringr)
library(janitor)
library(lubridate)
library(xml2)

tx_current <- read.csv("data/texas/texas_current.csv")

total <- sum(tx_current$Measure.Values)
today <- with_tz(Sys.time(), tzone = "America/Chicago")
today_pretty <- format(as.Date(today), "%b %d, %Y")

max_county <- tx_current$Home.County[which.max(tx_current$Measure.Values)]

tx_current_for_XML <- tx_current %>% 
  select(Home.County, LAT, LON, Measure.Values) %>% 
  rename(label = Home.County, 
         latitude = LAT, 
         longitude = LON, 
         value = Measure.Values) %>% 
  mutate(showLabel = case_when(label == max_county ~ 1,
                               TRUE ~ 0),
         showValue = 0)

#variables 
xml_title <- "Measles cases in Texas counties"
xml_subtitle <- " "
xml_maxValue <- max(tx_current$Measure.Values) #max value for dots
xml_source <- "Texas Department of State Health Services"
xml_date <- paste0("As of ", today_pretty)
xml_type <- "map" #line, bar, pie, etc
xml_geography <- "TX"
xml_qualifier <- " " #one line note, if needed


# Create chart node
chart_xml <- xml_new_root("chart")

#add children (title, subtitle, type)
xml_add_child(chart_xml, "title", xml_title)
xml_add_child(chart_xml, "subtitle", xml_subtitle)
xml_add_child(chart_xml, "type", xml_type)
xml_add_child(chart_xml, "maxValue", xml_maxValue)

# Add data rows
for (i in 1:nrow(tx_current_for_XML)) {
  row_node <- xml_add_child(chart_xml, "dataPoint")
  for (col_name in names(tx_current_for_XML)) {
    xml_add_child(row_node, col_name, as.character(tx_current_for_XML[i, col_name]))
  }
}


xml_add_child(chart_xml, "source", xml_source)
xml_add_child(chart_xml, "date", xml_date)
xml_add_child(chart_xml, "qualifier", xml_qualifier)

write_xml(chart_xml, "data/texas/texas-dot-map.xml")
