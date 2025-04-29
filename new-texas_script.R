library(httr)

url <- "https://tabexternal.dshs.texas.gov/t/THD/views/2025MeaslesOutbreakWebsite3/MeaslesReportPg1/crosstab.csv"

response <- GET(url)

if (status_code(response) == 200) {
  df <- read.csv(text = content(response, as = "text"))
  print(head(df))
} else {
  print("Failed to fetch data")
}