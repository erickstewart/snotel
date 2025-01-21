
library(tidyverse)

library(zoo)

library(scales)

library(ggrepel)
library(ggnewscale)
library(geomtextpath)
library(patchwork)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(reactable)
library(plotly)

library(snotelr)
library(openmeteo)

library(maps)
library(sf)

library(climaemet)


options(scipen = 999)

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

sn_hourly_function <- function(site_id, state, days){
  
  base_url <- paste0(
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/hourly/start_of_period/",
    site_id, ":",
    state, ":",
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

metric_to_imperial_function <- function(df){
  df %>%
    mutate(across(matches("snow"), ~ . / 25.4)) %>%
    mutate(across(matches("precip|temperature"), ~ (. * 9/5) + 32)) %>%
    mutate(across(matches("wind_speed|wind_gust"), ~ . / 1.60934))
  
}

imperial_to_metric_function <- function(df) {
  df %>%
    mutate(across(matches("snow"), ~ . * 25.4)) %>% 
    mutate(across(matches("precip|temperature"), ~ (. - 32) * 5/9)) %>%
    mutate(across(matches("wind_speed|wind_gust"), ~ . * 1.60934))
}

generate_acronym <- function(name) {
  words <- str_split(name, "\\s+")[[1]]
  str_to_upper(str_c(str_sub(words, 1, 1), collapse = ""))
}

flag_function <- function(df){
  df %>%
    mutate(flag_snow_change = case_when(
      snow_change_vs_lag_1day >= 8 | snow_change_vs_lag_3day >= 16 ~ 2,
      snow_change_vs_lag_1day >= 4 | snow_change_vs_lag_3day >= 8 ~ 1,
      .default = 0)) %>%
    mutate(flag_temp = case_when(
      temperature_max > 36 & snow_depth > 10 ~ 3,
      temperature_max > 33 & snow_depth > 10 ~ 2,
      temperature_max > 32 & snow_depth > 10 ~ 1,
      .default = 0))
}



flag_label_function <- function(df){
  df %>%
    mutate(label_snow_change = str_c(
      if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
      "Snow: ",
      round(snow_change_vs_lag_1day), ifelse(input$units == "imperial", "in", "mm"), "(1day)\n",
      round(snow_change_vs_lag_3day), ifelse(input$units == "imperial", "in", "mm"), "(3day)")
    ) %>%
    mutate(label_temp_flag = str_c(
      if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
      round(temperature_max), ifelse(input$units == "imperial", "F", "C"))
    )
}
