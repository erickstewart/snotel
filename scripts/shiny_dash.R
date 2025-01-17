#shiny data ####
shiny_vars <- tibble(var = c(
  "snow_depth",
  "snow_change_vs_lag01", "snow_change_vs_lag03", "snow_change_vs_lag07",
  "temperature_max",
  "temperature_mean",
  "temperature_min",
  "precipitation",
  "precipitation_cumulative",
  "snow_change_vs_lag12h", "snow_change_vs_lag24h", "snow_change_vs_lag36h"
)) %>%
  mutate(var_order = row_number())

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Visualization App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Snotel Daily", tabName = "daily_data", icon = icon("calendar")),
      menuItem("Snotel Hourly", tabName = "hourly_data", icon = icon("clock")),
      menuItem("Weather Forecast", tabName = "weather_data", icon = icon("cloud")),
      
      pickerInput("site_name", "Select Site Name(s):", 
                  choices = unique(bbs_meta$site_name),
                  selected = "mores creek summit",
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 10
                  ),
                  multiple = TRUE
      ),
      
      pickerInput("var", "Select Snotel Var(s):", 
                  choices = shiny_vars$var,
                  selected = "snow_depth",
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 10
                  ),
                  multiple = TRUE
      ),
      
      selectInput("units", "Units:", 
                  choices = c("imperial", "metric"), 
                  selected = "imperial"
      ),
      
      pickerInput("var2", "Select Weather Var(s):", 
                  choices = names(wf)[str_starts(names(wf), "hourly")],
                  selected = "hourly_snowfall",
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 10
                  ),
                  multiple = TRUE
      ),
      
      dateRangeInput("date_range1", "Snotel Date Range:",
                     min = min(daily$date), max = max(daily$date),
                     start = as.Date(max(daily$date)) - 21, end = max(daily$date)
      ),
      
      dateRangeInput("date_range2", "Weather Date Range:",
                     min = min(wf$date), max = max(wf$date),
                     start = (Sys.Date() - 2), end = max(wf$date)
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "daily_data",
              h2("Snotel Daily"),
              plotOutput("dailyPlot", height = "650px")
      ),
      
      tabItem(tabName = "hourly_data",
              h2("Snotel Hourly"),
              plotOutput("hourlyPlot", height = "650px")
      ),
      
      tabItem(tabName = "weather_data",
              h2("Weather Forecast"),
              plotOutput("wfPlot", height = "650px")
      )
    )
  )
)
# server ####
server <- function(input, output, session) {
  
  # Reactive data frames
  r_daily <- reactive({
    daily %>% 
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_ac, site_order),
                by = "site_id") %>% 
      filter(site_name %in% input$site_name) %>%
      group_by(site_id) %>%
      arrange(date) %>%
      mutate(snow_change_vs_lag01 = snow_depth - lag(snow_depth, 1),
             snow_change_vs_lag03 = snow_depth - lag(snow_depth, 3),
             snow_change_vs_lag07 = snow_depth - lag(snow_depth, 7)) %>%
      ungroup() %>%
      mutate(flag_snow_change = case_when(
        snow_change_vs_lag01 >= 200 | snow_change_vs_lag03 >= 400 ~ 2,
        snow_change_vs_lag01 >= 100 | snow_change_vs_lag03 >= 200 ~ 1,
        .default = 0)) %>%
      mutate(flag_temp = case_when(
        temperature_max > 1.65 & snow_depth / 25.4 > 10 ~ 3,
        temperature_max > 0.65 & snow_depth / 25.4 > 10 ~ 2,
        temperature_max > 0 & snow_depth / 25.4 > 10 ~ 1,
        .default = 0))  %>%
      {if(input$units == "imperial") imp_function(.) else . } %>%
      mutate(label_snow_change = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        round(snow_change_vs_lag01), ifelse(input$units == "imperial", "in", "mm"), "(1day); ",
        round(snow_change_vs_lag03), ifelse(input$units == "imperial", "in", "mm"), "(3day)")
      ) %>%
      mutate(label_temp_flag = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        round(temperature_max, 1), ifelse(input$units == "imperial", "F", "C"))
      ) %>%
      pivot_longer(c(starts_with("snow") | starts_with("temp")), names_to = "var", values_to = "value") %>%
      filter(var %in% input$var) %>%
      filter(date >= input$date_range1[1] & date <= input$date_range1[2]) %>%
      left_join(shiny_vars)
    
  })
  
  r_hourly <- reactive({
    hourly %>% 
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_ac, site_order),
                by = "site_id") %>% 
      filter(site_name %in% input$site_name) %>%
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
      mutate(snow_change_vs_lag12h = snow_depth - lag(snow_depth, 12),
             snow_change_vs_lag24h = snow_depth - lag(snow_depth, 24),
             snow_change_vs_lag36h = snow_depth - lag(snow_depth, 36)) %>%
      ungroup() %>%
      mutate(flag_snow_change = case_when(
        snow_change_vs_lag12h >= 200 | snow_change_vs_lag36h >= 400 ~ 2,
        snow_change_vs_lag12h >= 100 | snow_change_vs_lag36h >= 200 ~ 1,
        .default = 0)) %>%
      mutate(flag_temp = case_when(
        temperature_max > 1.65 & snow_depth / 25.4 > 10 ~ 3,
        temperature_max > 0.65 & snow_depth / 25.4 > 10 ~ 2,
        temperature_max > 0 & snow_depth / 25.4 > 10 ~ 1,
        .default = 0))  %>%
      {if(input$units == "metric") imp_to_metric_function(.) else . } %>%
      mutate(label_snow_change = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        round(snow_change_vs_lag12h), ifelse(input$units == "imperial", "in", "mm"), "(12h); ",
        round(snow_change_vs_lag36h), ifelse(input$units == "imperial", "in", "mm"), "(36h)")
      ) %>%
      mutate(label_temp_flag = str_c(
        if(length(input$site_name) > 1){str_c(site_ac, ": ")}, 
        round(temperature_max, 1), ifelse(input$units == "imperial", "F", "C"))
      ) %>%
      pivot_longer(c(starts_with("snow") | starts_with("temp")), names_to = "var", values_to = "value") %>%
      filter(var %in% input$var) %>%
      filter(date >= input$date_range1[1] & date <= input$date_range1[2]) %>%
      left_join(shiny_vars)
    
    
  })
  
  
  r_wf <- reactive({
    wf %>% 
      left_join(bbs_meta %>% 
                  select(site_id, site_name, site_ac, site_order),
                by = "site_id") %>% 
      filter(site_name %in% input$site_name) %>%
      pivot_longer(!c(date, hf, starts_with("site")), names_to = "var", values_to = "value") %>%
      filter(!is.na(value)) %>%
      filter(var %in% input$var2) %>%
      filter(date >= input$date_range2[1] & date <= input$date_range2[2]) 
    
    
  })
  
  
  
  # Daily plot
  output$dailyPlot <- renderPlot({
    r_daily() %>%
      ggplot(aes(x = date, y = value, 
                 group = reorder(site_name, site_order),
                 linetype = reorder(site_name, site_order),
                 alpha = reorder(site_name, site_order)
                 )) +
      {if(any(str_detect(input$var, "lag"))) list(geom_hline(yintercept = 0, size = 1.5, color = "gold"))} +
      geom_line(linewidth = 1) +
      geom_labelline(aes(label = site_name), linewidth = 1, size = 4, hjust = 0.9, show.legend = F) +
      scale_alpha_manual(values = c(1, 0.7, 0.5, 0.3, rep(0.3, 10))) +
      geom_label(data = .  %>% filter(flag_temp != 0),
                aes(color = as.character(flag_temp), label = label_temp_flag), 
                shape = 21, size = 4, nudge_y = -0.5, show.legend = F) +
      scale_color_manual(values = c("orange", "orangered", "red")) + 
      new_scale_color() +
      geom_label(data = .  %>% filter(flag_snow_change != 0),
                 aes(color = as.character(flag_snow_change), label = label_snow_change), 
                 shape = 21, size = 4, nudge_y = 0.5, show.legend = F) +
      scale_color_manual(values = c("skyblue1", "blue")) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~reorder(var, var_order), scales = "free_y", ncol = 1) +
      guides(alpha = "none") +
      labs(title = "Snotel - Daily Data", x = "Date", y = "Value", 
           subtitle = "Labels:\nDark blue = big storm; Light blue = med/small storm\nReds = Hot temps",
           linetype = "Snotel Site") +
      theme_minimal() +
      scale_x_date(date_breaks = "1 day", # Show every day
                   date_labels = "%Y-%m-%d") + # Format date labels
      theme(legend.position = "top",
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 13),
            strip.text = element_text(size = 13, face = "bold"),
            
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.minor.x = element_blank() # Remove minor gridlines
      )
    
  })
  
  # Hourly plot
  output$hourlyPlot <- renderPlot({
    r_hourly() %>%
      ggplot(aes(x = date, y = value, 
                 group = reorder(site_name, site_order),
                 linetype = reorder(site_name, site_order),
                 alpha = reorder(site_name, site_order)
      )) +
      {if(any(str_detect(input$var, "lag"))) list(geom_hline(yintercept = 0, size = 1.5, color = "gold"))} +
      geom_line(linewidth = 1) +
      geom_smooth(se = F, size = 0.8, show.legend = F) +
      scale_alpha_manual(values = c(1, 0.7, 0.5, 0.3, rep(0.3, 10))) +
      geom_label(data = .  %>% filter(flag_temp != 0),
                 aes(color = as.character(flag_temp), label = label_temp_flag), 
                 shape = 21, size = 4, nudge_y = -0.5, show.legend = F) +
      scale_color_manual(values = c("orange", "orangered", "red")) + 
      new_scale_color() +
      geom_label(data = .  %>% filter(flag_snow_change != 0),
                 aes(color = as.character(flag_snow_change), label = label_snow_change), 
                 shape = 21, size = 4, nudge_y = 0.5, show.legend = F) +
      scale_color_manual(values = c("skyblue1", "blue")) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~reorder(var, var_order), scales = "free_y", ncol = 1) +
      guides(alpha = "none") +
      labs(title = "Snotel - Hourly Data", x = "Date", y = "Value", 
           subtitle = "Labels:\nDark blue = big storm; Light blue = med/small storm\nReds = Hot temps",
           linetype = "Snotel Site") +
      theme_minimal() +
      scale_x_datetime(date_breaks = "6 hour", date_labels = "%Y-%m-%d %H:%M") + #Every 6 hours
      theme(legend.position = "top",
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 13),
            strip.text = element_text(size = 13, face = "bold"),
            
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.minor.x = element_blank() # Remove minor gridlines
      )
    
  })
  
  
  output$wfPlot <- renderPlot({
    r_wf() %>%
      ggplot(aes(x = date, y = value, 
                 group = interaction(hf, reorder(site_name, site_order)), 
                 linetype = reorder(site_name, site_order),
                 color = hf)
      ) +
      geom_line() +
      scale_color_manual(values = c("darkgreen", "darkgray"))+
      #geom_line(data = . %>% filter(hf == "historical")) +
      #geom_line(data = . %>% filter(hf != "historical")) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty_breaks(n = 10)(x)))) +
      facet_wrap(~var, scales = "free_y", ncol = 1) +
      labs(title = "Daily Data", x = "Date", y = "Value") +
      theme_minimal() +
      theme(legend.position = "top") +
      #scale_x_date(date_breaks = "1 day", # Show every day
      #             date_labels = "%Y-%m-%d") + # Format date labels
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.minor.x = element_blank() # Remove minor gridlines
      )
    
  })
}

# Run the app ####
shinyApp(ui, server)
