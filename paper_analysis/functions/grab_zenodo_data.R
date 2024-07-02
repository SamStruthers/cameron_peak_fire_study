
  data_folder <- list.files(path = "data", full.names = T, pattern = "ross_clp_chem")
  
  # Check if a folder with the specified name exists in the "data" folder
  if (is_empty(data_folder)) {
    # Download the entire folder from Zenodo DOI as a zipped folder in your working directory
    dwnload_file <- paste0("https://zenodo.org/records/", DOI, "/files/rossyndicate/CLP_waterchem_data-", data_version, ".zip?download=1")
    download.file(url = dwnload_file, destfile = 'data/ross_clp.zip') 
    
    # Unzip this file
    unzip('data/ross_clp.zip', exdir = "data/", overwrite = T) 
    
    # Grab the name of the download file from the current R project   
    unzipped_folder <- list.files(path = "data/", full.names = T) %>%
      keep(~ grepl("rossyndicate", .))
    #data folder to be renamed to
    data_folder <- "data/ross_clp_chem"
    #rename to standard file name
    file.rename(unzipped_folder, data_folder)
    
    # Grab the most recent cleaned and collated chemistry dataset
    # For the current release that file is `CPF_reservoir_chemistry_up_to_071023.csv`
    cleaned_chem_file <- list.files(path = paste0(data_folder,"/data/cleaned/"), full.names = TRUE, pattern = ".rds")
    site_meta_file <- list.files(path = paste0(data_folder, "/data/metadata/"), full.names = TRUE, pattern = "location_metadata")
    units_meta_file <- list.files(path = paste0( data_folder, "/data/metadata/"), full.names = TRUE, pattern = "units")
    
    most_recent_chem <- read_rds(cleaned_chem_file)
    most_recent_meta <- read_csv_arrow(site_meta_file)
    chem_units <- readxl::read_xlsx(units_meta_file)
    rm(cleaned_chem_file, site_meta_file, data_folder,dwnload_file)
  } else {
    #Only grab previously downloaded file
    cleaned_chem_file <- list.files(path = paste0(data_folder, "/data/cleaned/"), full.names = TRUE, pattern = ".rds")
    site_meta_file <- list.files(path = paste0( data_folder, "/data/metadata/"), full.names = TRUE, pattern = "location_metadata")
    units_meta_file <- list.files(path = paste0(data_folder, "/data/metadata/"), full.names = TRUE, pattern = "units")
    
    most_recent_chem <- readr::read_rds(cleaned_chem_file)
    most_recent_meta <- read_csv_arrow(site_meta_file)
    chem_units <- readxl::read_xlsx(units_meta_file)
    # Folder already exists, you may choose to print a message or take other actions
    cat("Data Version: ",data_version, "\nMake sure this is the most recent version of available data")
    rm(cleaned_chem_file, site_meta_file, data_folder, units_meta_file )
  }
  
  
  

