
wqs_raw_upload <- rio::import('custom_wqs_stds_2024-03-25.xlsx') %>% janitor::clean_names()


user_data_list$upload <- rio::import('curated_user_data_2024-04-23.xlsx')

if('preferredName' %in% colnames(user_data_list$upload) && 'dtxsid' %in% colnames(user_data_list$upload)){
  TRUE}else{FALSE}


# RAIS --------------------------------------------------------------------

rais <- rio::import(list.files(pattern = 'rais_curated_'), trust = T) %>% 
  mutate(
    enduse = 'Screening',
    subsource = NA_character_
  ) %>% 
  rename(
    Name = preferredName
  )


# Cust --------------------------------------------------------------------

cust_stds <- rio::import(
  list.files(pattern = 'custom_wqs_stds_'), na.strings=c(""," ","NA")) %>% 
  clean_names() %>% 
  rename(
    dtxsid = final,
    Name = preferred_name
  )

# SSWQS -------------------------------------------------------------------

sswqs <- rio::import(
  list.files(pattern = 
  'sswqs_curated_'), trust = TRUE
  ) %>% 
  mutate(
    criterion_id = as.numeric(criterion_id),
    criterion_id = paste0('sswqs_', criterion_id),
    subsource = 'SSWQS', 
    origin_category = 'State', 
    priority_id = 1, 
    data_category = 'primary',
    media = 'Surface water'
  ) %>% 
  rename(
    Name = preferredName,
    # criterion_value = crit_val_conv,
  )

# benchmarks --------------------------------------------------------------

benchmarks <- list_rbind(
  list(
    sswqs = sswqs, 
    cust_stds = cust_stds,  
    #ccd = hazard, 
    rais = rais
    ), names_to = 'data_source') %>% 
  mutate(
    criterion_value = case_when(
      unit_name == 'ug/l' ~ criterion_value/1000,
      unit_name == 'ug/L' ~ criterion_value/1000,
      unit_name == 'ng/l' ~ criterion_value/1e-6
      ,.default = criterion_value),
    
    unit_name = case_when(
      unit_name == 'ug/L' ~ 'mg/l',
      unit_name == 'ug/l' ~ 'mg/l',
      unit_name == 'ng/l' ~ 'mg/l',
      unit_name == 'ng/L' ~ 'mg/l',
      unit_name == 'mg/L' ~ 'mg/l'
      ,.default = unit_name),
    
    protection = case_when(
      is.na(protection) ~ 'UNC',
      protection == 'Human' ~ 'Human health',
      protection == 'human health' ~ 'Human health',
      protection == 'eco' ~ 'Ecological'
      ,.default = protection),
    
    media = case_when(
      is.na(media) ~ 'UNC',
      media == 'SurfaceWater' ~ 'Surface water'
      ,.default = media
    ),
    
    priority_id = case_when(
      source == 'RSL' ~ 3
      ,.default = priority_id)
  )
