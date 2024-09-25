dat <- ComptoxR::chemi_hazard(dtx_list$dtxsid)
tp <- ComptoxR::tp_combined_score(dat$records, id = 'dtxsid')

#plotting pie charts

t1 <- tp$tp_scores
t2 <- t1 %>% 
  left_join(dat$headers, ., by = 'dtxsid') %>% #name
  select(-c(score, dtxsid)) %>%
  rename(compound = name) %>% 
  #filter(compound %in% c('Benzene')) %>% 
  pivot_longer(., cols = !c(compound), values_to = 'score')


  t1 %>% 
  left_join(temp$headers, ., by = 'dtxsid') %>% #name
  select(-c(score, dtxsid)) %>%
  rename(compound = name) %>% 
  filter(compound %in% c('Benzene')) %>% 
  pivot_longer(., cols = !c(compound), values_to = 'score') %>% 
  {ggplot(.) +
    aes(x = name, y = score, fill = name) +
    geom_bar(stat="identity") +
    scale_fill_viridis_d(option = "viridis", direction = 1) +
    #coord_polar() +
    theme_void() +
    xlab(NULL) +
    labs(
      y = 'Score',
      fill = 'Endpoint') +
    facet_grid(~compound)} %>% 
    ggplotly(.)

#ranking
ggplotly(
  tp$data$tp_scores %>% 
    left_join(., temp$headers, by = 'dtxsid') %>%
    left_join(., tp$data$variable_coverage, by = 'dtxsid') %>% 
    #select(name, dt score, data_coverage) %>% 
    mutate(dtxsid = forcats::fct_reorder(dtxsid, score)) %>% 
    #mutate(name = forcats::fct_reorder(name, score)) %>% 
  ggplot(.) +
    aes(x = score, y = dtxsid, colour = data_coverage, text = name) +
    geom_point(shape = "circle", size = 3.45, alpha = 0.5) +
    scale_color_viridis_c(option = "viridis", direction = 1) +
    labs(
      x = "Score",
      y = "Compound",
      title = "Relative Risk Ranking",
      caption = "Color indicates data coverage on [0-1] scale",
      color = "Coverage Score"
    ) +
    theme_classic() +
    theme(
      axis.title.y = element_text(size = 14L),
      axis.title.x = element_text(size = 14L)
    )
)

#shiny compound plot

tp$tp_scores %>% 
  left_join(., dat$headers, by = 'dtxsid') %>%
  left_join(., tp$variable_coverage, by = 'dtxsid') %>% 
  arrange(desc(score)) %>% 
  mutate(name = forcats::fct_reorder(name, score)) %>% 
  select(!c(score, data_coverage, dtxsid)) %>% 
  pivot_longer(cols = !c(name), names_to = 'compound', values_to = 'endpoint') %>% 
  pivot_wider(id_cols = 'compound', names_from = 'name', values_from = 'endpoint') %>% 
  rename(Endpoint = compound)

####
