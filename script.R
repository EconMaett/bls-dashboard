#' ---
#' title: "R script file"
#' ---

library(httr2)
library(jsonlite)
library(pins)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)

# Environment variables ----

bls_key <- Sys.getenv("BLS_API_KEY")

# BLS tables -----
table_ids <- c(
  "CUUR0000SA0",
  "CUUR0000SA0L1E",
  "CUUR0000SAF1",
  "CUUR0000SA0E",
  "CUUR0000SETB01",
  "CUUR0000SAM",
  "CUUR0000SEMC01",
  "CUUR0000SEMD01",
  "CUUR0000SEMF01",
  "CUUR0000SAH1"
)

id_to_label <- c(
  "CUUR0000SA0" ~ "All groups CPI",
  "CUUR0000SA0L1E" ~ "All items less food and energy",
  "CUUR0000SAF1" ~ "Food",
  "CUUR0000SA0E" ~ "Energy",
  "CUUR0000SETB01" ~ "Gasoline",
  "CUUR0000SAM" ~ "Medical care",
  "CUUR0000SEMC01" ~ "Physicians' services",
  "CUUR0000SEMD01" ~ "Hospital services",
  "CUUR0000SEMF01" ~ "Prescription drugs",
  "CUUR0000SAH1" ~ "Shelter"
)

# Pull BLS data -----

get_bls_data <- function(parameters) {
  
  bls_resp <- request(base_url = "https://api.bls.gov/publicAPI/v2/timeseries/data/") |> 
    req_body_json(parameters, auto_unbox = TRUE) |> 
    req_perform()
  
  if(resp_status(bls_resp) != 200) {
    stop(paste("API Error:", resp_status(bls_resp)))
  }
  
  
  return(resp_body_json(bls_resp))
}


all_data <- list()


for(table_id in table_ids) {
  parameters <- list(
    registrationkey = bls_key,
    seriesid = list(table_id),
    startyear = "2019",
    endyear = year(today()),
    calculations = TRUE
  )
  
  bls_data_object <- get_bls_data(parameters)
  all_data[[table_id]] <- bls_data_object
}

file_path <- file.path(getwd(), "all_data_report.json")

write_json(all_data, path = file_path, pretty = TRUE)

dat <- fromJSON(file_path)


# Clean data ----

series_dat <- dat |> 
  map(.f = ~ .x$Results$series) |> 
  map(.f = ~ tibble(seriesID = .x$seriesID, data = .x$data)) |> 
  list_rbind()

combined_dat <- series_dat |> 
  unnest(data)

clean_dat <- combined_dat |> 
  mutate(
    year_month = ymd(paste(year, str_sub(period, 2, 3), "01", sep = "-")),
    value = as.numeric(value),
    seriesID = as.character(seriesID),
    category_label = case_match(seriesID, !!!id_to_label)
  )

january_2019_values <- clean_dat |> 
  filter(year_month == ymd("2019-01-01")) |> 
  select(seriesID, jan_2019_value = value) |> 
  distinct()

joined_dat <- clean_dat |> 
  left_join(y = january_2019_values, by = join_by(seriesID))

final_cpi_dat <- joined_dat |> 
  mutate(
    jan_2019_diff = value - jan_2019_value,
    jan_2019_pct_change = (jan_2019_diff / jan_2019_value) * 100
  ) |> 
  arrange(year_month) |> 
  mutate(percent_change_from_previous_month = (value / lag(value) - 1) * 100, .by = category_label)


# Save the data ----

saveRDS(object = final_cpi_dat, file = "bls-cpi-data.rds")
