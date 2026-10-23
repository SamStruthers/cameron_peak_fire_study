#Longitudinal plots
jwc_long_plot <- function(param = "ChlA", dot_size = 3, b_size = 20, facet_scales = "fixed"){
  
  med_col_name <- paste0("median_", param)
  ggplot() + 
    geom_point(data = jwc_longitudinal, aes(x = site_code, y = .data[[param]], color = season),
               size = dot_size, position = position_jitterdodge(jitter.height = 0))+
    geom_rect(data = median_data %>% filter(site_code %in% c("JOER", "CBRR")),
              aes(xmin = as.numeric(site_code) - 0.4, xmax = as.numeric(site_code) + 0.4, ymin = - Inf, ymax = Inf, group = Year),
              fill = "#59B851", alpha =0.2)+
    geom_rect(data = median_data %>% filter(site_code %in% c("JWC")),
              aes(xmin = as.numeric(site_code) - 0.4, xmax = as.numeric(site_code) + 0.4, ymin = - Inf, ymax = Inf, group = Year),
              fill = "#002FA7", alpha =0.2)+
    scale_color_manual(values = season_clp_color_vals)+
    geom_line(data = median_data, aes(x = site_code, y = .data[[med_col_name]], group = Year), color = "black", linewidth = 1) +
    geom_point(data = median_data, aes(x = site_code, y = .data[[med_col_name]], group = Year, shape = "Yearly Site\n Median"), color = "black", size = dot_size) +
    scale_shape_manual(name = "", values = c("Yearly Site\n Median" = 16)) + 
    guides(shape = guide_legend(override.aes = list(color = "black", size = 3))) +
    theme_bw(base_size = b_size)+
    facet_wrap(~Year, ncol = 1, scales = facet_scales)+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")+
    scale_x_discrete(labels = c( "JOEI"= "  Above\nJoe Wright", 
                                 "JOER" = "Joe Wright\nReservoir", 
                                 "CBRI" = "Above\nChambers",
                                 "CBRR" = "Chambers\nReservoir", 
                                 "CHD" = "Below\nChambers", 
                                 "JWC" = "Joe Wright Creek"))+
    labs(y = chem_expressions[[param]], 
         color = "Season")
}