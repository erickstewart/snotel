#state boundary ####
idaho_boundary <- map_data("state") %>%
  filter(region == "idaho")

oregon_boundary <- map_data("state") %>%
  filter(region == "oregon")

#meta data ####
site_meta_data <- snotel_info() %>% 
  mutate(across(c(site_name), str_trim)) 

id_meta = site_meta_data %>% 
  filter(state == "ID") 

#select
bbs_names = c(
  "mores creek summit"
  , "big creek summit"
  , "banner summit"
  , "bogus basin"
  , "galena summit"
  , "jackson peak"
  , "deadwood summit"
  , "dollarhide summit"
  , "trinity mtn"
  , "secesh summit"
  , "south mtn"
  , "magic mountain"
  , "bostetter rs"
  , "howell canyon"
)

#additional weather locations
wf_strings <- c(
  "Pilot Peak; 43.96; -115.69; 7976; ID",
  "Jughandle Mtn; 44.84; -115.97; 7152; ID",
  "Cape Horn Mtn; 44.4; -115.23; 8743; ID",
  "Anthony Lakes; 44.97; -118.23; 7129; OR",
  "Hayden Peak; 42.98; -116.65; 7867; ID",
  "Smoky Dome; 43.5; -114.93; 9593; ID"
)

wf_addl <- tibble(tmp = wf_strings) %>%
  separate_wider_delim(tmp, delim = "; ", names = c("site_name", "latitude", "longitude", "elev_ft", "state")) %>%
  mutate(across(!c(site_name, state), as.numeric)) %>%
  mutate(elev_meters = round(elev_ft / 3.28084))



bbs_meta = id_meta %>% 
  mutate(site_name = str_replace(site_name, "\\.", "")) %>%
  # filter(latitude <= 45.19) %>%
  # filter(longitude <= -114.42) %>%
  left_join(
    data.frame(site_name = bbs_names) %>%
      mutate(site_order = row_number())) %>%
  arrange(site_order, site_name) %>%
  bind_rows(wf_addl) %>%
  mutate(site_order = row_number()) %>%
  bind_rows(
    site_meta_data %>%
      filter(site_id %in% c("759", "477"))
  ) %>%
  filter(str_detect(site_name, paste(c(bbs_names, wf_addl$site_name), collapse = "|")) | 
           state == "OR") %>%
  mutate(site_name = str_to_title(site_name)) %>%
  mutate(word1 = word(site_name, 1),
         word2 = word(site_name, 2)) %>%
  mutate(count = n(), .by = word1) %>%
  mutate(site_ac = case_when(
    count == 1 | is.na(word2) ~ str_sub(word1, 1, 6),
    .default = str_c( str_sub(word1, 1, 3), "_", str_sub(word2, 1, 3)))
  ) %>%
  mutate(elev_meters = ifelse(is.na(elev_meters), elev, elev_meters)) %>%
  mutate(elev_ft = ifelse(is.na(elev_ft), round(elev * 3.28084), elev_ft)) %>%
  select(state, site_name, site_order, site_ac, site_id, elev_ft, elev_meters, latitude, longitude) %>%
  mutate(site_label = str_to_title(site_name),
         site_label = case_when(
           !is.na(site_id) ~ site_label,
           .default = str_c(site_label, "_not_snotel")
         )) %>%
  mutate(snotel_site = !is.na(site_id)) %>%
  mutate(site_name_f = factor(site_name, levels = unique(site_name)))




#snotel daily ####
daily_raw <- map_dfr(bbs_meta$site_id[!is.na(bbs_meta$site_id)], sn_daily_function)

daily <- daily_raw %>%
  mutate(date = as.Date(date)) %>%
  filter(year(date) >= year(Sys.Date()) - 10) %>%
  select(site_id:precipitation) %>%
  metric_to_imperial_function()


#snotel hourly ####
hourly_raw <- NULL
for(i in bbs_meta$site_id[!is.na(bbs_meta$site_id)]){
  s <- bbs_meta %>% filter(site_id == i) %>% pull(state) %>% str_to_lower()
  site_data <- sn_hourly_function(i, s, days = 14) %>% mutate(site_id = i)
  hourly_raw <- bind_rows(hourly_raw, site_data)
}


hourly <- hourly_raw %>%
  mutate(date = ymd_hm(date, tz = "America/Denver"))



#weather ####


weather_vars <- c(
  "temperature_2m",
  "precipitation",
  "snowfall",
  "wind_speed_10m",
  "wind_gusts_10m",
  "wind_direction_10m"
)


# wh_raw <- NULL
# 
# for(i in bbs_meta$site_id){
#   ll <- bbs_meta %>% filter(site_id == i)
#   
#   df <- openmeteo::weather_history(
#     location = c(ll$latitude, ll$longitude),
#     hourly = weather_vars,
#     response_units = list(temperature_unit = "fahrenheit",
#                           precipitation_unit = "inch",
#                           windspeed_unit = "mph"),
#     start = (Sys.Date() - 14),
#     end = Sys.Date(),
#     timezone = "America/Denver"
#   )
#   wh_raw <- bind_rows(wh_raw, df %>% mutate(site_id = i))
# }

wf_raw <- NULL

for(i in bbs_meta$site_name){
  ll <- bbs_meta %>% filter(site_name == i)
  
  df <- openmeteo::weather_forecast(
    location = c(ll$latitude, ll$longitude),
    hourly = weather_vars,
    response_units = list(temperature_unit = "fahrenheit",
                          precipitation_unit = "inch",
                          windspeed_unit = "mph"),
    start = (Sys.Date() - 14),
    end = Sys.Date() + 7,
    timezone = "America/Denver"
  )
  wf_raw <- bind_rows(wf_raw, df %>% mutate(site_name = i))
}

wf <- wf_raw %>%
  mutate(date = as.POSIXct(format(ymd_h(str_c(str_sub(
    datetime, 1, 10), " ", hour(datetime))), "%Y-%m-%d %H:%M"))) %>%
  mutate(hf = case_when(
    as.Date(date) < Sys.Date() ~ "historical",
    .default = "forecast"
  )) %>%
  select(-datetime) %>%
  rename_with(~ str_replace_all(., "hourly_|_10m|_2m", "")) %>%
  group_by(site_name) %>%
  arrange(site_name, date) %>%
  mutate(snowfall_rolling_12h_sum = rollsum(snowfall, k = 12, align = "right", fill = NA)) %>%
  ungroup()


