#meta data ####
site_meta_data <- snotel_info()

id_meta = site_meta_data %>% 
  filter(state == "ID") %>% 
  mutate(across(c(site_name), str_trim)) %>%
  mutate(word1 = word(site_name, 1),
         word2 = word(site_name, 2)) %>%
  mutate(count = n(), .by = word1) %>%
  mutate(site_ac = case_when(
    count == 1 | is.na(word2) ~ str_sub(word1, 1, 6),
    .default = str_c( str_sub(word1, 1, 3), "_", str_sub(word2, 1, 3)))
  )

#select
bbs_names = c(
  "mores creek summit"
  , "big creek summit"
  , "banner summit"
  , "bogus basin"
  , "galena summit"
)

bbs_meta = id_meta %>% 
  filter(latitude <= 45.19) %>%
  filter(longitude <= -114.42) %>%
  #filter(str_detect(site_name, paste(bbs_names, "|"))) %>%
  #mutate(site_ac = map_chr(site_name, generate_acronym)) %>%
  left_join(
    data.frame(site_name = bbs_names) %>%
      mutate(site_order = row_number())) %>%
  arrange(site_order, site_name) %>%
  mutate(site_order = row_number()) %>%
  select(site_name, site_order, site_ac, site_id, elev, latitude, longitude)




#snotel daily ####
daily_raw <- map_dfr(bbs_meta$site_id[1:5], sn_daily_function)

daily <- daily_raw %>%
  mutate(date = as.Date(date)) %>%
  select(site_id:precipitation) %>%
  filter(year(date) >= 2022) 


#snotel hourly ####
hourly_raw <- NULL
for(i in bbs_meta$site_id){
  site_data <- sn_hourly_function(i, days = 7) %>% mutate(site_id = i)
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
  "wind_gusts_10m"
)


wh_raw <- NULL

for(i in bbs_meta$site_id){
  ll <- bbs_meta %>% filter(site_id == i)
  
  df <- openmeteo::weather_history(
    location = c(ll$latitude, ll$longitude),
    hourly = weather_vars,
    start = (Sys.Date() - 14),
    end = Sys.Date()
  )
  wh_raw <- bind_rows(wh_raw, df %>% mutate(site_id = i))
}

wf_raw <- NULL

for(i in bbs_meta$site_id){
  ll <- bbs_meta %>% filter(site_id == i)
  
  df <- openmeteo::weather_forecast(
    location = c(ll$latitude, ll$longitude),
    hourly = weather_vars
  )
  wf_raw <- bind_rows(wf_raw, df %>% mutate(site_id = i))
}

wf <- wh_raw %>%
  mutate(hf = "historical") %>%
  filter(datetime < min(wf_raw$datetime)) %>%
  bind_rows(wf_raw %>% mutate(hf = "forecast")) %>%
  mutate(date = format(ymd_h(str_c(str_sub(datetime, 1, 10), " ", hour(datetime))), "%Y-%m-%d %H:%M")) %>%
  select(-datetime)

