
  library(tidyverse)
  library(sf)
  library(nhdplusTools)
  
  sf::sf_use_s2(FALSE)
  
  # Download some NHDPlusHR Data specific to Poudre Region
  hr_data <- nhdplusTools::download_nhdplushr( "data/nhd_flowlines/", 1019)
  hr_flowlines <- nhdplusTools::get_nhdplushr(hr_data, layers = c("NHDFlowline"))
  hr_catchments <- nhdplusTools::get_nhdplushr(hr_data, layers = c("NHDPlusCatchment"))

  #Sites file
  site_file <- "data/ross_clp_chem/data/metadata/location_metadata.csv"
  
  # Read in sites
  sites <- read_csv(site_file)
  
  # 
  # for(i in 1:nrow(sites)){
  #   ms_delineate_watershed(
  #     lat = sites$Lat[i],
  #     lon = sites$Long[i],
  #     write_dir = "data/spatial/cpf_catchments_shapefiles/", 
  #     write_name = sites$site_code[i]
  #   )
  # }
  
  
  # Get the index of the flowline that is closest to the site
  start_index <- get_flowline_index(hr_flowlines$NHDFlowline,
                                    sites,
                                    search_radius = 10)
  #generate watersheds based on the COMID using NHD_plus data downloaded
  watersheds <-  map(start_index$COMID, function(x){
    
    ids <- get_UT(hr_flowlines$NHDFlowline, x)
    
    ws <- hr_catchments[[1]] %>%
      filter(FEATUREID %in% ids) %>%
      filter(FEATUREID != 23001900058842) %>%
      summarize() %>%
      nngeo::st_remove_holes() %>%
      sf::st_transform(26913)
    
    return(ws)
    
  })
  
  #Sanity check for watershed delineation
  #does not include any transbasin diversions
  mapview::mapview(watersheds, alpha.regions = 0.001)

  site_nums <- sites %>%
    mutate(site_no = 1:n())%>%
    st_drop_geometry()
  
  
  # export watersheds to shapefile by mapping over the list of watersheds
  for(i in 1:length(watersheds)){
    site_name <- site_nums$site_code[i]
    sf::st_write(watersheds[[i]], paste0("data/spatial/cpf_catchments_shapefiles/", site_name, ".shp"), append = F)
  }
 
  #read in a test catchment
  
  test_catchment <- function(site){
    test <- st_read(paste0("data/spatial/cpf_catchments_shapefiles/", site, ".shp"))
    mapview::mapview(test)+
      mapview::mapview(sites%>%filter(site_code == site),  color = "red", size = 5)
    
  }
#check sites  
  
  test_catchment("CBRI")

  # #site to download Soil Burn Severity for the Cameron Peak fire BAER report
  # https://mtbs.gov/direct-download

  
  sbs <- raster::raster("data/spatial/BAER_sbs_raster/cpf_sbs_BAER.tif") 
 # remove water bodies 
  sbs[sbs == 6] <- NA
  
# plot with BS colors
  #raster::plot(sbs, col = c("white", "darkgreen", "cyan", "yellow", "red", "green2"))

  legend <- tibble(value = c(1,2,3,4,5, 6),
                   legend = c("Very Low/Unburned", "Low", "Moderate", "High", "Unburned", "waterbodies"))
  
  # shapefile of watershed(s)
  ws <- watersheds %>%
    bind_rows() %>%
    #mutate(id = c(1,2,3,4,5,6,7))
    cbind(tibble(index= sites$site_code))
  
  # using the exactextractr package, get what is essentially a "zonal histogram" in
  # ArcGIS speak
  ws_sbs <- exactextractr::exact_extract(sbs, ws,
                                         append_cols = T,
                                         summarize_df = T,
                                         fun=function(x) x %>%
                                           group_by(sbs_code = as.character(value)) %>%
                                           summarise(count = sum(coverage_fraction))) %>%
    group_by(index) %>% 
    mutate(total = sum(count)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'sbs_code', values_from = 'count',
                names_glue = "sbs{sbs_code}" ) %>%
    inner_join(ws, .) -> ws_sbs
  
  ws_percents <- ws_sbs %>% 
    mutate(Unburned=100*(sbs0/total),
           V_low=100*(sbs1/total),
           Low=100*(sbs2/total),
           Moderate=100*(sbs3/total),
           High=100*(sbs4/total)) %>%
    select(index, Unburned, V_low, Low, Moderate, High) %>%
    ungroup()
  
  write_csv(ws_percents, "data/sbs_watershed.csv")

# #create a weighted score for area surrounding each reservoirs 
# buffer_sbs <- read_csv('data/sbs_buffer.csv') %>%
#   mutate(Buffer_Level=((Unburned*0)+(V_Low*0.1)+(Low*0.4)+(Moderate*0.7)+(High*1))/(Unburned+V_Low+Low+Moderate+High))

#create a weighted score for all watershed area burned 
watershed_sbs <- read_csv('data/sbs_watershed.csv') %>%
  mutate(Watershed_Level=((Unburned*0)+(V_Low*0.1)+(Low*0.4)+(Moderate*0.7)+(High*1))/(Unburned+V_Low+Low+Moderate+High)) %>% rename(Site_Code=SITE_CODE)