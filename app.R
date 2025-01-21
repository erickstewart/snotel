#source ####
library(here)

source(here("scripts", "utils.R"))
source(here("scripts", "data.R"))

#shiny text ####
text_sn_daily <- "Select Snotel site(s), Snotel variables, and Snotel date range 
Storm labels: Dark blue = big storm; Light blue = med/small storm
Temp labels: Reds = High temps (max daily)
"
text_sn_hourly <- "- Select Snotel site(s), Snotel variables, and Snotel date range. 
- Storm labels: Dark blue = big storm; Light blue = med/small storm
- Temp labels: Reds = High temps
"

text_wf <- "- Select Snotel and/or other sites, Weather variables, and Weather date range"

text_doc1 <- "Purpose of this app is to provide me (and perhaps others) a one-stop shop for snotel and weather data
Scope is the Boise area backcountry skiing region
"

text_doc2 <- "- Available sites are shown in the meta table tab. There are snotel and non-snotel sites
- Daily snotel data is pulled from 2024-01-01 on
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


#theme
theme_sn <- theme(
  plot.title = element_text(size = 14, face = "bold"),
  plot.subtitle = element_text(size = 13),
  legend.position = "right",
  legend.text = element_text(size = 13),
  strip.text = element_text(size = 13, face = "bold"),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11),
  panel.grid.minor.x = element_blank()
)

# #ui ####
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Boise Backcountry Skiing App"),
#   
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Snotel Daily", tabName = "daily_data", icon = icon("calendar")),
#       menuItem("Snotel Hourly", tabName = "hourly_data", icon = icon("clock")),
#       menuItem("Weather Forecast", tabName = "weather_data", icon = icon("cloud")),
#       menuItem("Wind Direction", tabName = "wind_dir", icon = icon("cloud")),
#       menuItem("Meta Table", tabName = "meta_table", icon = icon("table")),
#       menuItem("Map of ID/OR", tabName = "map_idaho", icon = icon("map")),
#       menuItem("Documentation", tabName = "documentation", icon = icon("book")),
#       
#       
#       selectInput("units", "Units:", 
#                   choices = c("imperial", "metric"), 
#                   selected = "imperial"
#       ),
#       
#       pickerInput("site_label", "Select Site Name (multiple):", 
#                   choices = unique(bbs_meta$site_label),
#                   selected = "Mores Creek Summit",
#                   options = list(
#                     `actions-box` = TRUE,
#                     `live-search` = TRUE,
#                     size = 10
#                   ),
#                   multiple = TRUE
#       ),
#       
#       pickerInput("sn_var", "Select Snotel Var (multiple):", 
#                   choices = shiny_vars$var,
#                   selected = "snow_depth",
#                   options = list(
#                     `actions-box` = TRUE,
#                     `live-search` = TRUE,
#                     size = 10
#                   ),
#                   multiple = TRUE
#       ),
#       
#       pickerInput("wf_var", "Select Weather Forecast Var(s):", 
#                   choices = c(
#                     "snowfall", "snowfall_rolling_6h_sum",
#                     "precipitation",
#                     "temperature",                                      
#                     "wind_speed", "wind_gusts"
#                     ),
#                   selected = c("snowfall_rolling_6h_sum", "temperature"),
#                   options = list(
#                     `actions-box` = TRUE,
#                     `live-search` = TRUE,
#                     size = 10
#                   ),
#                   multiple = TRUE
#       ),
#       
#       sliderInput(
#         inputId = "date_range1",
#         label = "Snotel Date Range:",
#         min = min(daily$date),
#         max = max(daily$date),
#         value = c(max(daily$date)-21, max(daily$date))
#       ),
#       sliderInput(
#         inputId = "date_range2",
#         label = "Weather Forecast Date Range:",
#         min = as.Date(min(wf$date)),
#         max = as.Date(max(wf$date)),
#         value = c(Sys.Date() - 3, as.Date(max(wf$date)))
#       )
#     )
#   ),
#   
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "daily_data",
#               #h2("Snotel Daily"),
#               textAreaInput("doc_sn_daily", "Snotel Daily - Notes:", 
#                             placeholder = text_sn_daily, 
#                             width = "100%", height = "70px"),
#               plotOutput("dailyPlot", height = "600px")
#       ),
#       
#       tabItem(tabName = "hourly_data",
#               textAreaInput("doc_sn_hourly", "Snotel Hourly - Notes:", 
#                             placeholder = text_sn_hourly, 
#                             width = "100%", height = "70px"),
#               plotOutput("hourlyPlot", height = "600px")
#       ),
#       
#       tabItem(tabName = "weather_data",
#               #h2("Weather Forecast"),
#               textAreaInput("doc_wf", "Weather Forecast - Notes:", 
#                             placeholder = text_wf, 
#                             width = "100%", height = "70px"),
#               plotOutput("wfPlot", height = "600px")
#       ),
#       
#       tabItem(tabName = "wind_dir",
#               #h2("Wind Direction"),
#               plotOutput("windrose", height = "650px")
#       ),
#       
#       tabItem(tabName = "meta_table",
#               #h2("Site Meta Table"),
#               reactableOutput("metaTable")
#       ),
#       
#       tabItem(tabName = "map_idaho",
#               #h2("Map of Idaho & Oregon"),
#               plotOutput("idahoMap", height = "650px")
#       ),
#       
# 
#       tabItem(tabName = "documentation",
#               h2("Documentation"),
#               textAreaInput("doc_1", "Section 1:", 
#                             placeholder = text_doc1, 
#                             width = "100%", height = "150px"),  
#               textAreaInput("doc_2", "Section 2:", 
#                             placeholder = text_doc2, 
#                             width = "100%", height = "150px"),  
#               textAreaInput("doc_3", "Section 3:", 
#                             placeholder = text_doc3, 
#                             width = "100%", height = "150px"),  
#               textAreaInput("doc_4", "Section 4:", 
#                             placeholder = text_doc4, 
#                             width = "100%", height = "150px")  
#       )
#       
#     )
#   )
# )
# 
#ui mobile ####
library(shiny)
library(shinyWidgets)
library(reactable)
library(shinythemes)
library(shinydashboard)

ui <- fluidPage(
  titlePanel("Boise Backcountry Skiing App"),
  theme = shinythemes::shinytheme("flatly"),
  
  sidebarLayout( # Use sidebarLayout for a true sidebar
    sidebarPanel( # All inputs go in sidebarPanel
      width = 3, # Set the width of the sidebar
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
      
      pickerInput("wf_var", "Select Weather Forecast Var(s):",
                  choices = c(
                    "snowfall", "snowfall_rolling_6h_sum",
                    "precipitation", "temperature",
                    "wind_speed", "wind_gusts"),
                  selected = c("snowfall_rolling_6h_sum", "temperature"),
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 10
                  ),
                  multiple = TRUE),
      
      sliderInput(
        inputId = "date_range1",
        label = "Snotel Date Range:",
        min = min(daily$date),
        max = max(daily$date),
        value = c(max(daily$date)-21, max(daily$date))
      ),
      
      sliderInput(
        inputId = "date_range2",
        label = "Weather Forecast Date Range:",
        min = as.Date(min(wf$date)),
        max = as.Date(max(wf$date)),
        value = c(Sys.Date() - 3, as.Date(max(wf$date)))
      )
    ),
    
    mainPanel( # Tabs and content go in mainPanel
      width = 9, # Set the width of the main panel
      navbarPage(
        title = "",
        tabPanel("Snotel Daily",
                 plotOutput("dailyPlot", height = "600px")
        ),
        tabPanel("Snotel Hourly",
                 plotOutput("hourlyPlot", height = "600px")
        ),
        tabPanel("Weather Forecast",
                 plotOutput("wfPlot", height = "600px")
        ),
        tabPanel("Wind Direction",
                 plotOutput("windrose", height = "650px")
        ),
        tabPanel("Meta Table",
                 reactableOutput("metaTable")
        ),
        tabPanel("Map of ID/OR",
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
                 textAreaInput("doc_wf", "Weather Forecasts:",
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
                  select(site_id, site_name, site_label, site_ac, site_order),
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
        round(snow_change_vs_lag_1day), ifelse(input$units == "imperial", "in", "mm"), "(1day); ",
        round(snow_change_vs_lag_3day), ifelse(input$units == "imperial", "in", "mm"), "(3day)")
      ) %>%
      mutate(label_temp_flag = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        round(temperature_max), ifelse(input$units == "imperial", "F", "C"))
      ) %>%
      pivot_longer(c(starts_with("snow") | starts_with("temp") | starts_with("precip")), 
                   names_to = "var", values_to = "value") %>%
      filter(str_detect(var, paste(input$sn_var, collapse = "|"))) %>%
      filter(var != "temperature_mean") %>%
      filter(date >= input$date_range1[1] & date <= input$date_range1[2]) %>%
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
  
  r_hourly <- reactive({
    hourly %>% 
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_label, site_ac, site_order),
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
        round(snow_change_vs_lag_1day), ifelse(input$units == "imperial", "in", "mm"), "(1day); ",
        round(snow_change_vs_lag_3day), ifelse(input$units == "imperial", "in", "mm"), "(3day)")
      ) %>%
      mutate(label_temp_flag = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        round(temperature_max), ifelse(input$units == "imperial", "F", "C"))
      ) %>%
      pivot_longer(c(starts_with("snow") | starts_with("temp") | starts_with("precip")), 
                   names_to = "var", values_to = "value") %>%
      filter(str_detect(var, paste(input$sn_var, collapse = "|"))) %>%
      filter(var != "temperature_mean") %>%
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
      mutate(var_units = str_c(var, " (", var_units, ")"))
    
    
  })
  
  
  r_wf <- reactive({
    wf %>% 
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_label, site_ac, site_order),
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
  #windrose
  output$windrose <- renderPlot({
    
    speed_cuts <- c(0, 10, 20, 30, 40)
    if(input$units == "metric") {speed_cuts <- speed_cuts * 1.60934}
    
    gd0 <- wf %>%
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_label, site_ac, site_order)) %>% 
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
      theme_sn 
    
    g2 <- ggwindrose(speed = gd2$wind_speed, direction = gd2$wind_direction, facet = gd2$site_name,  ncol = 2,
                     speed_cuts = speed_cuts, 
                     legend_title = if(input$units == "imperial"){"Wind Speed (mph)"} else {"Wind Speed (kmh)"}
                     ) +
      labs(title = "Wind Direction - Next 24 hours",
           subtitle = str_c(min(gd2$date), " to ", max(gd2$date))) +
      theme_linedraw() +
      theme_sn +
      theme(title = element_text(color = "darkgray"))
    

    g1 + g2
    
  })
  
  
  # Daily plot
  output$dailyPlot <- renderPlot({

    r_daily() %>%
      ggplot(aes(x = date, y = value, 
                 group = reorder(site_label, site_order),
                 linetype = reorder(site_label, site_order),
                 alpha = reorder(site_label, site_order)
      )) +
      geom_line(
        data = . %>% filter(str_detect(var, "lag")) %>% 
          mutate(temp_line = 0), 
        aes(y = temp_line, group = 1),
        color = "gold",
        alpha = 1,
        linetype = "dashed"
      ) +
      geom_line(
        data = . %>% filter(str_detect(var, "temperature_min")) %>%
          mutate(temp_line = ifelse(input$units == "imperial", 0, -17.8)),
        aes(y = temp_line, group = 1),
        color = "steelblue",
        alpha = 1,
        linetype = "dashed"
      ) +
      geom_ribbon(
        data = . %>% filter(str_detect(var, "temperature_max")),
        aes(ymin = if(input$units == "imperial"){32} else {0}, ymax = if(input$units == "imperial"){40} else {4.5}),
        fill = "red", color = NA,
        alpha = 0.2,
        show.legend = F
      ) +
      geom_line(linewidth = 1) +
      geom_labelline(aes(label = site_name), linewidth = 1, size = 3.5, hjust = 0.9, show.legend = F) +
      # geom_text(data = . %>% filter(!is.na(date)) %>% filter(date == max(date), .by = var),
      #           aes(label = site_ac), hjust = 0.5, vjust = -0.5) +
      scale_alpha_manual(values = c(1, 0.7, 0.5, 0.3, rep(0.3, 10))) +
      geom_label_repel(data = .  %>% filter(flag_temp != 0 & !str_detect(var, "min|mean")),
                       aes(color = as.character(flag_temp), label = label_temp_flag), 
                       shape = 21, size = 4, nudge_y = -0.5, show.legend = F) +
      scale_color_manual(values = c("orange", "orangered", "red")) + 
      new_scale_color() +
      geom_label(data = .  %>% filter(flag_snow_change != 0),
                 aes(color = as.character(flag_snow_change), label = label_snow_change), 
                 shape = 21, size = 4, nudge_y = 0.5, show.legend = F) +
      scale_color_manual(values = c("skyblue1", "blue")) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~reorder(var_units, var_order), scales = "free_y", 
                 ncol = if(length(unique(r_daily()$var)) <= 3){1} else {2}
                 ) +
      guides(alpha = "none") +
      labs(title = "Snotel - Daily Data", 
           subtitle = "Storm labels: Dark blue = big storm; Light blue = med/small storm\nTemp labels: Reds = High temps (max daily)",
           x = NULL, 
           y = NULL,
           linetype = "Snotel Site") +
      scale_x_date(date_breaks = "1 day", # Show every day
                   date_labels = "%Y-%m-%d") + 
      theme_linedraw() +
      theme_sn
    
  })
  
  # Hourly plot
  output$hourlyPlot <- renderPlot({
    r_hourly() %>%
      ggplot(aes(x = date, y = value, 
                 group = reorder(site_label, site_order),
                 linetype = reorder(site_label, site_order),
                 alpha = reorder(site_label, site_order)
      )) +
      geom_line(
        data = . %>% filter(str_detect(var, "lag")) %>% 
          mutate(temp_line = 0), 
        aes(y = temp_line, group = 1),
        color = "gold",
        alpha = 1,
        linetype = "dashed"
      ) +
      geom_line(
        data = . %>% filter(str_detect(var, "temperature_min")) %>%
          mutate(temp_line = ifelse(input$units == "imperial", 0, -17.8)),
        aes(y = temp_line, group = 1),
        color = "steelblue",
        alpha = 1,
        linetype = "dashed"
      ) +
      geom_ribbon(
        data = . %>% filter(str_detect(var, "temperature_max")),
        aes(ymin = if(input$units == "imperial"){32} else {0}, ymax = if(input$units == "imperial"){40} else {4.5}),
        fill = "red", color = NA,
        alpha = 0.2,
        show.legend = F
      ) +
      geom_line(linewidth = 1) +
      #geom_smooth(se = F, size = 0.8, show.legend = F) +
      scale_alpha_manual(values = c(1, 0.7, 0.5, 0.3, rep(0.3, 10))) +
      geom_label_repel(data = .  %>% 
                         filter(!str_detect(var, "min|mean")) %>%
                         group_by(site_id, var) %>%
                         arrange(date) %>%
                         mutate(row = row_number()) %>%
                         mutate(label_group = ifelse(flag_temp != 0 & lag(flag_temp) == 0, row, NA)) %>% 
                         fill(label_group, .direction = "down") %>%
                         ungroup() %>%
                         
                         group_by(site_id, var, label_group) %>%
                         #slice(1, n()) %>% # Keep only the first row within each group
                         filter(row_number() == 1 | label_temp_flag == max(label_temp_flag)) %>%
                         ungroup() %>%
                         filter(flag_temp != 0)
                       
                         
                         ,
                       aes(color = as.character(flag_temp), label = label_temp_flag), 
                       shape = 21, size = 4, nudge_y = -0.5, show.legend = F) +
      scale_color_manual(values = c("orange", "orangered", "red")) + 
      new_scale_color() +
      geom_label(data = .  %>% filter(flag_snow_change != 0),
                 aes(color = as.character(flag_snow_change), label = label_snow_change), 
                 shape = 21, size = 4, nudge_y = 0.5, show.legend = F) +
      scale_color_manual(values = c("skyblue1", "blue")) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~reorder(var_units, var_order), scales = "free_y", 
                 ncol = if(length(unique(r_hourly()$var)) <= 3){1} else {2}) +
      guides(alpha = "none") +
      labs(title = "Snotel - Hourly Data", 
           subtitle = "Storm labels: Dark blue = big storm; Light blue = med/small storm\nTemp labels: Reds = High temps",
           x = NULL, y = NULL, 
           linetype = "Snotel Site") +
      theme_linedraw() +
      scale_x_datetime(date_breaks = "6 hour", date_labels = "%Y-%m-%d %H:%M") + #Every 6 hours
      theme_sn
    
  })
  
  #weather forecast
  output$wfPlot <- renderPlot({
    r_wf() %>%
      ggplot(aes(x = date, y = value, 
                 group = interaction(hf, reorder(site_label, site_order)), 
                 linetype = reorder(site_label, site_order),
                 color = hf)
      ) +
      geom_line(
        data = . %>% filter(str_detect(var, "temp")) %>%
          mutate(temp_line = ifelse(input$units == "imperial", 0, -17.8)),
        aes(y = temp_line, group = 1),
        color = "steelblue",
        alpha = 1,
        linetype = "dashed"
      ) +
      geom_ribbon(
        data = . %>% filter(str_detect(var, "temp")),
        aes(ymin = if(input$units == "imperial"){32} else {0}, ymax = if(input$units == "imperial"){40} else {4.5}),
        fill = "red", color = NA,
        alpha = 0.2,
        show.legend = F
      ) +
      
      geom_vline(xintercept = Sys.time(), color = "purple", linewidth = 1) +
      geom_text(
        data = r_wf() %>% group_by(var) %>% summarize(max_y = 0.9*max(value, na.rm = TRUE)),
        aes(x = Sys.time(), y = max_y, label = "now"),
        inherit.aes = FALSE,
        color = "purple",
        vjust = -0.5
      ) +     
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("darkgray", "black"))+
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~var_units, scales = "free_y", ncol = if(length(input$wf_var) <= 3){1} else {2}) +
      labs(title = "Weather Data - Hourly", x = NULL, y = NULL,
           color = "Historical or Forecast",
           linetype = "Site Name") +
      theme_linedraw() +
      scale_x_datetime(date_breaks = "6 hour", date_labels = "%Y-%m-%d %H:%M") + 
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
      scale_color_manual(values = c("blue", "snow")) +
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
shinyApp(ui, server)
