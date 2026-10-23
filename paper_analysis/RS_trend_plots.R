#RS Trends plors

graphTrendObs_SDD = function(lake) {
  seasonAdjTrends <- all_rs %>% 
    mutate(doy = yday(date))%>%
    filter(site_code == lake & !is.na(mean_sdd) & (doy >= 120 & doy <= 300))%>%
    filter(!c(site_code %in% c("HORR", "COMR") & year(date) %in% c(2021,2022)))%>% 
    select(gnis_name, date, mean_sdd)%>%
    as_tibble() %>% 
    arrange(date) %>%
    time_decompose(mean_sdd, merge = T) 
  
  ggplot(seasonAdjTrends, aes(x = date, y = observed)) +
    geom_point(color = 'gray') +
    geom_vline(xintercept = as.Date(c("2020-08-01")), linetype = "dashed", color = "red", lwd = 1) +
    # Removed title and y-axis title from here so they don't double up
    labs(x = NULL, y = NULL, title = NULL) + 
    geom_point(aes(y = trend), color = '#E69F00') +
    geom_smooth(aes(y = trend), lty = 2) +
    theme_bw(base_size = 14) +
    ylim(0,6)+
    scale_y_continuous(position = "right", 
                       #add labels every 2 from 0-6
                       breaks = seq(0, 6, by = 2), 
                       limits = c(0,6))+
    ROSS_theme
  
}

graphTrendObs_temp = function(lake) {
  seasonAdjTrends <- all_rs %>% 
    mutate(doy = yday(date))%>%
    filter(site_code == lake & !is.na(med_surface_C) & (doy >= 120 & doy <= 300))%>%
    filter(!c(site_code %in% c("HORR", "COMR") & year(date) %in% c(2021,2022)))%>% 
    select(gnis_name, date, med_surface_C)%>%
    as_tibble()%>% 
    arrange(date)%>%
    time_decompose(med_surface_C, merge = T)
  
  lakeName = seasonAdjTrends$gnis_name[1]
  
  ggplot(seasonAdjTrends, aes(x = date, y = observed)) +
    geom_point(color = 'gray') +
    geom_vline(xintercept = as.Date(c("2020-08-01")), linetype = "dashed", color = "red", lwd = 1) +
    # Removed y-axis title from here
    labs(x = NULL, y = NULL, title = paste0(lakeName)) + 
    geom_point(aes(y = trend), color = '#E69F00') +
    geom_smooth(aes(y = trend), lty = 2) +
    theme_bw(base_size = 14) + # Dropped base_size slightly so labels fit better side-by-side
    theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 12)) +
    ylim(0,30)+
    ROSS_theme
}