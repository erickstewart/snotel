install.packages("zoo")

library(tidyverse)

library(zoo)

library(scales)

library(ggrepel)
library(ggnewscale)
library(geomtextpath)

library(shiny)
library(shinyWidgets)
library(shinydashboard)

library(snotelr)
library(openmeteo)


#sn_daily_function
sn_daily_function <- function(site_id){
  df <- snotel_download(
    site_id = site_id,
    internal = TRUE
  )
}


# define new column names
snotel_columns <- c(
  "date",
  "snow_water_equivalent",
  "precipitation_cumulative",
  "temperature_max",
  "temperature_min",
  "temperature_mean",
  "precipitation",
  "snow_depth"
)

sn_hourly_function <- function(site_id, days){
  
  base_url <- paste0(
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/hourly/start_of_period/",
    site_id, ":",
    "id", ":",
    "sntl",
    "%7Cid=\"\"%7Cname/-",
    round(days*24,0),
    ",0/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value,SNWD::value"
  )
  
  
  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(getwd(),
                                                       "snotel_tmp.csv"),
                                      overwrite = TRUE))
  
  # read in the snotel data
  df <- utils::read.table(file.path(getwd(),"snotel_tmp.csv"),
                          header = TRUE,
                          sep = ",",
                          stringsAsFactors = FALSE)
  
  colnames(df) <- snotel_columns
  
  return(df)
  
}

units_function <- function(df){
  df %>%
      
    mutate(units = case_when(
      str_starts(metric, "temp") ~ "celsius",
      .default = "mm")) 
}

imp_function_old <- function(df){
  df %>%
    mutate(value = case_when(
      str_starts(var, "temp") ~ (value * 9/5) + 32,
      .default = value / 25.4))
}

imp_function <- function(df){
  df %>%
    mutate(across(starts_with("snow") | starts_with("temp"), ~ case_when(
      str_starts(cur_column(), "temp") ~ (.x * 9/5) + 32,
      .default = .x / 25.4)))
}

imp_to_metric_function <- function(df) {
  df %>%
    mutate(across(starts_with("snow") | starts_with("temp"), ~ case_when(
      str_starts(cur_column(), "temp") ~ (.x - 32) * 5/9, # Fahrenheit to Celsius
      TRUE ~ .x * 25.4 # Inches to cm/mm
    )))
}

generate_acronym <- function(name) {
  words <- str_split(name, "\\s+")[[1]]
  str_to_upper(str_c(str_sub(words, 1, 1), collapse = ""))
}

flag_function <- function(df){
  df %>%
    mutate(flag_snow_change = case_when(
      snow_change_vs_lag01 >= 200 | snow_change_vs_lag03 >= 400 ~ 2,
      snow_change_vs_lag01 >= 100 | snow_change_vs_lag03 >= 200 ~ 1,
      .default = 0)) %>%
    mutate(flag_temp = case_when(
      temperature_max > 1.65 & snow_depth / 25.4 > 10 ~ 3,
      temperature_max > 0.65 & snow_depth / 25.4 > 10 ~ 2,
      temperature_max > 0 & snow_depth / 25.4 > 10 ~ 1,
      .default = 0)) 
}