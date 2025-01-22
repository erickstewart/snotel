#libraries #####
library(here)

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

#source ####

source(here("scripts", "utils.R"))
source(here("scripts", "data.R"))

#shiny dates ####
dates_seasons <- daily %>%
  select(date) %>%
  distinct() %>%
  mutate(
    month_day = str_sub(as.character(date), 6, 10),
    month = month(date),
    year = as.character(year(date))
  ) %>%
  mutate(season = case_when(
    month %in% c(9:12) ~ str_c(year, "_", as.character(as.numeric(year) + 1)),
    .default = str_c(as.character(as.numeric(year) -1), "_", year)
  )) %>%
  mutate(date_order = case_when(
    month %in% c(9:12) ~ 1,
    .default = 2
  ))

dates_month_day <- dates_seasons %>%
  select(month_day, date_order) %>%
  distinct() %>%
  arrange(date_order, month_day)


#shiny text ####
text_labels <- "Blue labels: snow; day-over-day change in snow depth
Red labels: high temps; orange > 32F; orangered > 33F; red > 36F"

text_sn_daily <- "Select Snotel site(s), Snotel variables, and Snotel date range 
Blue labels: snow; day-over-day change in snow depth
Red labels: high temps; orange > 32F; orangered > 33F; red > 36F
"
text_sn_hourly <- "Select Snotel site(s), Snotel variables, and Snotel date range. 
Blue labels: snow; day-over-day change in snow depth
Red labels: high temps; orange > 32F; orangered > 33F; red > 36F
"

text_wf <- "Select Snotel and/or other sites, Weather variables, and Weather date range"


text_doc1 <- "The purpose of this app is to provide me (and perhaps others) a one-stop shop for snotel and weather data.
The current scope is the Boise area backcountry skiing region (roughly defined).
"

text_doc2 <- "- Available sites are shown in the Meta and Map tabs. There are Snotel and non-snotel sites.
- Daily snotel data is available for the last 10 years
- Hourly snotel is pulled for last 14 days
- Weather data is pulled from previous 14 and out 7 days
"

text_doc3 <- "Acknowledgements:
- snotelR package
- Joshua L Erickson https://rdrr.io/github/joshualerickson/wildlandhydRo/src/R/download_SNOTEL.R
- https://open-meteo.com/ and openmeteo package
"

text_doc4 <-"contact: eric.k.stewart@outlook.com
"

#shiny data ####
shiny_vars <- tibble(var = c(
  "snow_depth",
  "snow_change_vs_lag_1day", "snow_change_vs_lag_2day", "snow_change_vs_lag_3day", "snow_change_vs_lag_7day",
  "temperature",
  #"temperature_max",
  #"temperature_mean",
  #"temperature_min",
  "precipitation"
)) %>%
  mutate(var_order = row_number())


#theme ####
theme_sn <- theme(
  plot.title = element_text(size = 14, face = "bold"),
  plot.subtitle = element_text(size = 13),
  legend.position = "right",
  legend.text = element_text(size = 13),
  strip.text = element_text(size = 13, face = "bold"),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
  panel.grid.minor.x = element_blank()
)

#shiny colors ####
colors_sites <- c(
  "navyblue", "springgreen4", "khaki4", "grey20", "purple3", "gray70", 
  "royalblue", "springgreen1", "khaki1", "grey40", "purple1",  "black"
)
#ui mobile ####
ui <- fluidPage(
  
  
  #titlePanel("Boise Backcountry Skiing App"),
  theme = shinythemes::shinytheme("flatly"),
  
  sidebarLayout( # Use sidebarLayout for a true sidebar
    sidebarPanel( # All inputs go in sidebarPanel
      width = 3, # Set the width of the sidebar
      #h2("Boise Backcountry Skiing App"),
      tags$h4(style = "color: white; font-size: 20px; font-weight: bold; background-color: #2c3e50; padding: 5px;", "Boise Backcountry Skiing App"),
      selectInput("units", "Units:",
                  choices = c("imperial", "metric"),
                  selected = "imperial"),
      pickerInput("site_label", "Select Site Name (multiple):",
                  choices = unique(bbs_meta$site_label),
                  selected = "Mores Creek Summit",
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 10
                  ),
                  multiple = TRUE),
      
      pickerInput("sn_var", "Select Snotel Var (multiple):",
                  choices = shiny_vars$var,
                  selected = "snow_depth",
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 10
                  ),
                  multiple = TRUE),
      
      pickerInput("wf_var", "Select Weather Var(s):",
                  choices = c(
                    "snowfall", "snowfall_rolling_12h_sum",
                    "precipitation", "temperature",
                    "wind_speed", "wind_gusts"),
                  selected = c("snowfall_rolling_12h_sum", "temperature"),
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 10
                  ),
                  multiple = TRUE),
      
      dateRangeInput(
        inputId = "date_range1",
        label = "Snotel Date Range:",
        start = max(daily$date)-21,
        end = max(daily$date),
        min = min(daily$date),
        max = max(daily$date),
        startview = "year"
        #value = c(max(daily$date)-21, max(daily$date))
      ),
      
      sliderInput(
        inputId = "date_range2",
        label = "Weather Date Range:",
        min = as.Date(min(wf$date)),
        max = as.Date(max(wf$date)),
        value = c(Sys.Date() - 3, as.Date(max(wf$date)))
      ),
      
      
      pickerInput(
        inputId = "seasons",
        label = "Select Seasons to Compare Snotel:",
        choices = sort(unique(dates_seasons$season), decreasing = T),
        selected = tail(sort(unique(dates_seasons$season)), 2),
        options = list(
          `actions-box` = TRUE,
          #`live-search` = TRUE,
          size = 10
        ),
        multiple = TRUE
      )
      #, colorSelectorInput("color1", "Select Color 1", choices = "red")
      
    ),
    
    
    mainPanel( # Tabs and content go in mainPanel
      width = 9, # Set the width of the main panel
      navbarPage(
        title = "",
        tabPanel("Snotel Daily",
                 plotlyOutput("dailyPlot", height = "600px"),
                 textAreaInput("doc_sn_daily", "Snotel Labels:",
                               placeholder = text_labels, 
                               width = "100%", height = "60px")
        ),
        tabPanel("Snotel Hourly",
                 plotlyOutput("hourlyPlot", height = "600px"),
                 textAreaInput("doc_sn_hourly", "Snotel Labels:",
                               placeholder = text_labels, 
                               width = "100%", height = "60px")
        ),
        tabPanel("Weather",
                 plotlyOutput("wfPlot", height = "650px")
        ),
        tabPanel("Wind",
                 plotOutput("windrose", height = "650px")
        ),
        tabPanel("Snotel Prior Seasons",
                 plotlyOutput("seasonsPlot", height = "600px")
        ),
        tabPanel("Meta Data",
                 reactableOutput("metaTable")
        ),
        tabPanel("Map",
                 plotOutput("idahoMap", height = "650px")
        ),
        tabPanel("Directions",
                 h2("Directions"),
                 textAreaInput("doc_sn_daily", "Snotel Daily:",
                               placeholder = text_sn_daily, 
                               width = "100%", height = "150px"),
                 textAreaInput("doc_sn_hourly", "Snotel Hourly:",
                               placeholder = text_sn_hourly,
                               width = "100%", height = "150px"),
                 textAreaInput("doc_wf", "Weather Data:",
                               placeholder = text_wf,
                               width = "100%", height = "150px"),
        ),
        
        tabPanel("Documentation",
                 h2("Documentation"),
                 textAreaInput("doc_1", "Section 1:",
                               placeholder = text_doc1,
                               width = "100%", height = "150px"),
                 textAreaInput("doc_2", "Section 2:",
                               placeholder = text_doc2,
                               width = "100%", height = "150px"),
                 textAreaInput("doc_3", "Section 3:",
                               placeholder = text_doc3,
                               width = "100%", height = "150px"),
                 textAreaInput("doc_4", "Section 4:",
                               placeholder = text_doc4,
                               width = "100%", height = "150px")
        )
      )
    )
  )
)

#server ####
server <- function(input, output, session) {
  
  
  
  # Reactive data frames
  
  r_daily <- reactive({
    daily %>% 
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_name_f, site_label, site_ac, site_order),
                by = "site_id") %>% 
      filter(site_label %in% input$site_label) %>%
      group_by(site_id) %>%
      arrange(date) %>%
      mutate(snow_change_vs_lag_1day = snow_depth - lag(snow_depth, 1),
             snow_change_vs_lag_2day = snow_depth - lag(snow_depth, 2),
             snow_change_vs_lag_3day = snow_depth - lag(snow_depth, 3),
             snow_change_vs_lag_7day = snow_depth - lag(snow_depth, 7)) %>%
      ungroup() %>%
      flag_function() %>%
      {if(input$units == "metric") imperial_to_metric_function(.) else . } %>%
      mutate(label_snow_change = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        "Snow: ",
        round(snow_change_vs_lag_1day), ifelse(input$units == "imperial", "in", "mm"), "(1day)")
        #round(snow_change_vs_lag_3day), ifelse(input$units == "imperial", "in", "mm"), "(3day)")
      ) %>%
      mutate(label_temp_flag = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        round(temperature_max), ifelse(input$units == "imperial", "F", "C"))
      ) %>%
      pivot_longer(c(starts_with("snow") | starts_with("temp") | starts_with("precip")), 
                   names_to = "var", values_to = "value") %>%
      filter(str_detect(var, paste(input$sn_var, collapse = "|"))) %>%
      filter(date >= input$date_range1[1] & date <= input$date_range1[2]) %>%
      left_join(shiny_vars) %>%
      mutate(var_units = case_when(
        str_detect(var, "snow|precip") & input$units == "imperial" ~ "inches",
        str_detect(var, "snow|precip") & input$units == "metric" ~ "mm",
        input$units == "imperial" ~ "F",
        input$units == "metric" ~ "C",
        .default = "other"
      )) %>%
      mutate(var_units = str_c(var, " (", var_units, ")")) %>%
      filter(var != "temperature_mean") 
    
  })
  
  r_hourly <- reactive({
    hourly %>% 
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_name_f, site_label, site_ac, site_order),
                by = "site_id") %>% 
      filter(site_label %in% input$site_label) %>%
      group_by(site_id) %>%
      arrange(date) %>%
      mutate(across(starts_with("snow"), ~na.approx(.))) %>%
      mutate(across(starts_with("snow"), ~case_when(
        lead(.x) == lag(.x) ~ lag(.),
        lead(.x) == lag(.x, 2) ~ lag(., 2),
        lead(.x, 2) == lag(.x) ~ lag(.),
        lead(.x, 2) == lag(.x, 2) ~ lag(., 2),
        .default = .x))) %>%
      mutate(across(starts_with("snow"), ~rollmean(., k = 3, fill = NA, align = "center"))) %>%
      mutate(
        snow_change_vs_lag_1day = snow_depth - lag(snow_depth, 24),
        snow_change_vs_lag_2day = snow_depth - lag(snow_depth, 48),
        snow_change_vs_lag_3day = snow_depth - lag(snow_depth, 72),
        snow_change_vs_lag_7day = snow_depth - lag(snow_depth, 7*24),
      ) %>%
      ungroup() %>%
      flag_function() %>%
      {if(input$units == "metric") imperial_to_metric_function(.) else . } %>%
      mutate(label_snow_change = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        round(snow_change_vs_lag_1day), ifelse(input$units == "imperial", "in", "mm"), "(1day)")
        #round(snow_change_vs_lag_3day), ifelse(input$units == "imperial", "in", "mm"), "(3day)")
      ) %>%
      mutate(label_temp_flag = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        round(temperature_max), ifelse(input$units == "imperial", "F", "C"))
      ) %>%
      pivot_longer(c(starts_with("snow") | starts_with("temp") | starts_with("precip")), 
                   names_to = "var", values_to = "value") %>%
      filter(str_detect(var, paste(input$sn_var, collapse = "|"))) %>%
      filter(date >= input$date_range1[1] & date <= input$date_range1[2]) %>%
      left_join(shiny_vars) %>%
      mutate(units = case_when(
        str_detect(var, "snow|precip") & input$units == "imperial" ~ "inches",
        str_detect(var, "snow|precip") & input$units == "metric" ~ "mm",
        input$units == "imperial" ~ "degrees (F)",
        input$units == "metric" ~ "degrees (C)",
        .default = "other"
      )) %>%
      mutate(var_units = case_when(
        str_detect(var, "snow|precip") & input$units == "imperial" ~ "inches",
        str_detect(var, "snow|precip") & input$units == "metric" ~ "mm",
        input$units == "imperial" ~ "degrees (F)",
        input$units == "metric" ~ "degrees (C)",
        .default = "other"
      )) %>%
      mutate(var_units = str_c(var, " (", var_units, ")")) %>%
      filter(var != "temperature_mean")
    
    
    
  })
  
  
  r_wf <- reactive({
    wf %>% 
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_name_f, site_label, site_ac, site_order),
                by = "site_name") %>% 
      filter(site_label %in% input$site_label) %>%
      {if(input$units == "metric") imperial_to_metric_function(.) else . } %>%
      pivot_longer(!c(date, hf, starts_with("site")), names_to = "var", values_to = "value") %>%
      filter(!is.na(value)) %>%
      filter(var %in% input$wf_var) %>%
      filter(date >= input$date_range2[1] & date <= input$date_range2[2]) %>%
      mutate(var_units = case_when(
        str_detect(var, "wind_speed|wind_gust") & input$units == "imperial" ~ "mph",
        str_detect(var, "wind_speed|wind_gust") & input$units == "metric" ~ "kmh",
        str_detect(var, "snow|precip") & input$units == "imperial" ~ "inches",
        str_detect(var, "snow|precip") & input$units == "metric" ~ "mm",
        input$units == "imperial" ~ "F",
        input$units == "metric" ~ "C",
        .default = "other"
      )) %>%
      mutate(var_units = str_c(var, " (", var_units, ")"))
  })
  
  
  
  r_seasons <- reactive({
    daily %>% 
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_name_f, site_label, site_ac, site_order),
                by = "site_id") %>% 
      filter(site_label %in% input$site_label) %>%
      {if(input$units == "metric") imperial_to_metric_function(.) else . } %>%
      pivot_longer(c(starts_with("snow") | starts_with("temp") | starts_with("precip")), 
                   names_to = "var", values_to = "value") %>%
      filter(str_detect(var, paste(input$sn_var, collapse = "|"))) %>%
      left_join(dates_seasons) %>%
      filter(season %in% as.character(input$seasons)) %>%
      left_join(shiny_vars) %>%
      mutate(var_units = case_when(
        str_detect(var, "snow|precip") & input$units == "imperial" ~ "inches",
        str_detect(var, "snow|precip") & input$units == "metric" ~ "mm",
        input$units == "imperial" ~ "F",
        input$units == "metric" ~ "C",
        .default = "other"
      )) %>%
      mutate(var_units = str_c(var, " (", var_units, ")"))
  })
  
  
  
  # Daily plot
  output$dailyPlot <- renderPlotly({
    
    r_daily() %>%
      filter(var != "temperature_mean") %>%
      ggplot(aes(x = date, y = value,
                 group = site_name_f,
                 color = site_name_f
      )) +
      {if(any(str_detect(input$var_sn, "lag"))) list(
        geom_line(
          data = . %>% filter(str_detect(var, "lag")) %>%
            mutate(temp_line = 0),
          aes(y = temp_line, group = 1),
          color = "gold",
          alpha = 1,
          linetype = "dashed"
        )
      )
      } +
      geom_ribbon(
        data = . %>% filter(str_detect(var, "temperature_min") & !is.na(value)),
        aes(ymin = if(input$units == "imperial"){min(0, min(value))} else {min(-17.8, min(value))},
            ymax = if(input$units == "imperial"){5} else {-15}),
        fill = "steelblue", color = NA,
        alpha = 0.2,
        show.legend = F
      ) +
      geom_ribbon(
        data = . %>% filter(str_detect(var, "temperature_max") & !is.na(value)),
        aes(ymin = if(input$units == "imperial"){32} else {0}, 
            ymax = if(input$units == "imperial"){max(40, max(value))} else {max(4.5, max(value))}),
        fill = "red", color = NA,
        alpha = 0.2,
        show.legend = F
      ) +
      geom_point(data = .  %>% filter(flag_temp == 1),
                 color = "orange", size = 2, show.legend = F) +
      geom_point(data = .  %>% filter(flag_temp == 2),
                 color = "orangered", size = 2, show.legend = F) +
      geom_point(data = .  %>% filter(flag_temp == 3),
                 color = "red", size = 2, show.legend = F) +
      geom_line() +
      geom_point(size = 1) +
      scale_color_manual(values = colors_sites) +
      geom_text(data = . %>% filter(!is.na(date)) %>% filter(date == max(date), .by = var),
                aes(y = value * 1.005, label = site_ac) 
                #hjust = 0.5, vjust = 0.5
      ) +
      geom_text(data = .  %>% filter(flag_temp == 1 & !str_detect(var, "min|mean")),
                aes(label = label_temp_flag), 
                color = "orange", size = 3, nudge_y = -0.5, show.legend = F) +
      geom_text(data = .  %>% filter(flag_temp == 2 & !str_detect(var, "min|mean")),
                aes(label = label_temp_flag), 
                color = "orangered", size = 3, nudge_y = -0.5, show.legend = F) +
      geom_text(data = .  %>% filter(flag_temp == 3 & !str_detect(var, "min|mean")),
                aes(label = label_temp_flag), 
                color = "red", size = 3, nudge_y = -0.5, show.legend = F) +
      geom_text(data = .  %>% filter(flag_snow_change != 0),
                aes(label = label_snow_change),
                color = "deepskyblue", size = 3, nudge_y = 0.5, angle = 45, show.legend = F) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~reorder(var_units, var_order), scales = "free_y", 
                 ncol = if(length(unique(r_daily()$var)) <= 3){1} else {2}
      ) +
      guides(alpha = "none") +
      labs(#title = "Snotel - Daily Data", 
        #subtitle = "Storm labels: Dark blue = big storm; Light blue = med/small storm\nTemp labels: Reds = High temps (max daily)",
        x = NULL, 
        y = NULL,
        color = "Snotel Site") +
      scale_x_date(date_breaks = "1 day", # Show every day
                   date_labels = "%Y-%m-%d") + 
      theme_bw() +
      theme_sn
    
  })
  
  # Hourly plot
  output$hourlyPlot <- renderPlotly({
    
    r_hourly() %>%
      filter(var != "temperature_mean") %>%
      ggplot(aes(x = date, y = value,
                 group = site_name_f,
                 color = site_name_f
      )) +
      {if(any(str_detect(input$var_sn, "lag"))) list(
        geom_line(
          data = . %>% filter(str_detect(var, "lag")) %>%
            mutate(temp_line = 0),
          aes(y = temp_line, group = 1),
          color = "gold",
          alpha = 1,
          linetype = "dashed"
        )
      )
      } +
      geom_ribbon(
        data = . %>% filter(str_detect(var, "temperature_min") & !is.na(value)),
        aes(ymin = if(input$units == "imperial"){min(0, min(value))} else {min(-17.8, min(value))},
            ymax = if(input$units == "imperial"){5} else {-15}),
        fill = "steelblue", color = NA,
        alpha = 0.2,
        show.legend = F
      ) +
      geom_ribbon(
        data = . %>% filter(str_detect(var, "temperature_max") & !is.na(value)),
        aes(ymin = if(input$units == "imperial"){32} else {0}, 
            ymax = if(input$units == "imperial"){max(40, max(value))} else {max(4.5, max(value))}),
        fill = "red", color = NA,
        alpha = 0.2,
        show.legend = F
      ) +
      geom_point(data = .  %>% filter(flag_temp == 1),
                 color = "orange", size = 2, show.legend = F) +
      geom_point(data = .  %>% filter(flag_temp == 2),
                 color = "orangered", size = 2, show.legend = F) +
      geom_point(data = .  %>% filter(flag_temp == 3),
                 color = "red", size = 2, show.legend = F) +
      geom_line() +
      scale_color_manual(values = colors_sites) +
      geom_text(data = . %>% filter(!is.na(date)) %>% filter(date == max(date), .by = var),
                aes(y = value * 1.005, label = site_ac) 
                #hjust = 0.5, vjust = 0.5
      ) +
      geom_text(data = .  %>% filter(flag_snow_change != 0),
                aes(label = label_snow_change),
                color = "deepskyblue", size = 3, nudge_y = 0.5, angle = 45, show.legend = F) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~reorder(var_units, var_order), scales = "free_y", 
                 ncol = if(length(unique(r_hourly()$var)) <= 3){1} else {2}
      ) +
      labs(#title = "Snotel - Hourly Data", 
        x = NULL, 
        y = NULL,
        color = "Snotel Site") +
      scale_x_datetime(date_breaks = "12 hour", date_labels = "%Y-%m-%d %H:%M") + 
      theme_bw() +
      theme_sn
    
  })
  
  #weather forecast
  output$wfPlot <- renderPlotly({
    r_wf() %>%
      ggplot(aes(x = date, y = value, 
                 group = site_name_f, 
                 color = site_name_f)
      ) +
      geom_ribbon(
        data = . %>% filter(str_detect(var, "temp") & !is.na(value)),
        aes(ymin = if(input$units == "imperial"){min(0, min(value))} else {min(-17.8, min(value))},
            ymax = if(input$units == "imperial"){5} else {-15}),
        fill = "steelblue", color = NA,
        alpha = 0.2,
        show.legend = F
      ) +
      geom_ribbon(
        data = . %>% filter(str_detect(var, "temp") & !is.na(value)),
        aes(ymin = if(input$units == "imperial"){32} else {0}, 
            ymax = if(input$units == "imperial"){max(40, max(value))} else {max(4.5, max(value))}),
        fill = "red", color = NA,
        alpha = 0.2,
        show.legend = F
      ) +
      geom_vline(xintercept = as.numeric(floor_date(Sys.time(), "hour")), color = "gold") +
      geom_text(
        aes(x = floor_date(Sys.time(), "hour"),
            y = max(value, na.rm = TRUE), # Find max y value for each facet
            label = paste0(format(Sys.time(), "%Y-%m-%d %H:"), "00", "  now")),  # Format date and time
        inherit.aes = FALSE,
        color = "gold",
        size = 2.5,
        hjust = -0.5, 
        vjust = 1.1  
      ) +
      geom_line() +
      scale_color_manual(values = colors_sites) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~var_units, scales = "free_y", ncol = if(length(input$wf_var) <= 3){1} else {2}) +
      labs(#title = "Weather Data - Hourly", 
        x = NULL, y = NULL) +
      theme_bw() +
      scale_x_datetime(date_breaks = "12 hour", date_labels = "%Y-%m-%d %H:%M") + 
      theme_sn
    
  })
  
  
  #windrose
  output$windrose <- renderPlot({
    
    speed_cuts <- c(0, 10, 20, 30, 40)
    if(input$units == "metric") {speed_cuts <- speed_cuts * 1.60934}
    
    gd0 <- wf %>%
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_name_f, site_label, site_ac, site_order)) %>% 
      filter(site_label %in% input$site_label) %>%
      arrange(site_order, site_name)
    
    gd1 <- gd0 %>%
      filter(date < Sys.time() & date >= Sys.time() - hours(72))
    
    
    gd2 <- gd0 %>%
      filter(date >= Sys.time() & date >= Sys.time() + hours(24))
    
    g1 <- ggwindrose(speed = gd1$wind_speed, direction = gd1$wind_direction, facet = gd1$site_name, ncol = 2,
                     speed_cuts = speed_cuts, 
                     legend_title = if(input$units == "imperial"){"Wind Speed (mph)"} else {"Wind Speed (kmh)"}
    ) +
      labs(title = "Wind Direction - Previous 72 hours",
           subtitle = str_c(min(gd1$date), " to ", max(gd1$date))) +
      theme_linedraw() +
      theme_sn +
      theme(title = element_text(color = "darkred"))
    
    
    g2 <- ggwindrose(speed = gd2$wind_speed, direction = gd2$wind_direction, facet = gd2$site_name,  ncol = 2,
                     speed_cuts = speed_cuts, 
                     legend_title = if(input$units == "imperial"){"Wind Speed (mph)"} else {"Wind Speed (kmh)"}
    ) +
      labs(title = "Wind Direction - Next 24 hours",
           subtitle = str_c(min(gd2$date), " to ", max(gd2$date))) +
      theme_linedraw() +
      theme_sn +
      theme(title = element_text(color = "darkgreen"))
    
    
    g1 + g2
    
  })
  
  
  
  
  output$seasonsPlot <- renderPlotly({
    
    r_seasons() %>% 
      filter(var != "temperature_mean") %>%
      ggplot(aes(x = reorder(month_day, date_order), y = value,
                 #group = reorder(site_label, site_order),
                 #color = reorder(site_label, site_order)
                 group = interaction(site_name_f, season),
                 color = site_name_f,
                 linetype = reorder(season, desc(season)),
                 alpha = reorder(season, desc(season))
                 
                 #alpha = season
      )) +
      geom_vline(xintercept = which(str_detect(dates_month_day$month_day, "12-31")), color = "gold3") +
      geom_text(
        aes(x = "01-01",
            y = max(value, na.rm = TRUE),
            label = "New Year"),  # Format date and time
        inherit.aes = FALSE,
        color = "gold3",
        hjust = -0.5, # Adjust horizontal position
        vjust = 1.1  # Adjust vertical position
      ) +
      geom_line() +
      scale_color_manual(values = colors_sites) +
      scale_alpha_manual(values = c(1, rep(0.6, 20))) +
      # geom_text(data = . %>% filter(!is.na(date)) %>% filter(date == max(date), .by = var),
      #           aes(y = value * 1.005, label = site_ac) 
      #           #hjust = 0.5, vjust = 0.5
      # ) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~reorder(var_units, var_order), scales = "free_y", 
                 ncol = if(length(unique(r_daily()$var)) <= 3){1} else {2}
      ) +
      guides(alpha = "none") +
      labs(#title = "Snotel - Daily Data", 
        #subtitle = "Storm labels: Dark blue = big storm; Light blue = med/small storm\nTemp labels: Reds = High temps (max daily)",
        x = NULL, 
        y = NULL,
        color = "Snotel Site",
        linetype = "Snow Season") +
      # scale_x_date(date_breaks = "1 day", # Show every day
      #              date_labels = "%Y-%m-%d") +
      scale_x_discrete(breaks = function(x) dates_month_day$month_day[seq(1, length(dates_month_day$month_day), by = 14)]) +
      theme_bw() +
      theme_sn
    
  })
  
  
  
  # Map of Idaho
  output$idahoMap <- renderPlot({
    
    ggplot() +
      geom_polygon(data = idaho_boundary, aes(x = long, y = lat),
                   fill = "darkred", color = "black", alpha = 0.5) +
      geom_polygon(data = oregon_boundary, aes(x = long, y = lat),
                   fill = "seagreen", color = "black", alpha = 0.5) +
      geom_point(data = bbs_meta,
                 aes(x = longitude, y = latitude, 
                     color = snotel_site, shape = snotel_site, size = elev_ft)) +
      geom_text_repel(data = bbs_meta,
                      aes(x = longitude, y = latitude, 
                          color = snotel_site,
                          label = paste(str_to_title(site_name), "(", elev_ft, "ft)")),
                      size = 3.5,
                      show.legend = F) +
      scale_color_manual(values = c("blue", "gray90")) +
      scale_shape_manual(values = c(17, 19)) +
      #scale_size_continuous(limits = c(1.5, 3.5)) +
      coord_quickmap(xlim = range(c(oregon_boundary$long, idaho_boundary$long)), 
                     ylim = range(c(oregon_boundary$lat, idaho_boundary$lat))) + # Important!
      labs(title = "BBS Sites", x = "Longitude", y = "Latitude") +
      theme_bw()
  })
  
  # Meta Table
  output$metaTable <- renderReactable({
    reactable(bbs_meta %>%
                mutate(site_name = str_to_title(site_name)) %>%
                select(site_name, site_id, state, elev_ft, elev_meters, latitude, longitude, snotel_site),
              defaultPageSize = 20)
  })
  
}

#app ####
shinyApp(ui = ui, server = server)

