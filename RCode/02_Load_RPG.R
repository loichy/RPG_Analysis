PrepareRPGData <- function(i){

  # i <- 129
  # i <- 82
  # i <- 182
  
  # Print starting message
  print(paste("Iteration ", i, " for region ",all_rpg_links$region_name[i], " and for year ", all_rpg_links$year[i]))
  
  # Create objects useful for iteration i
  region_i <- all_rpg_links$region_name[i]
  region_code_i <- all_rpg_links$region_code[i]
  year_i <- all_rpg_links$year[i]
  url_i <- all_rpg_links$url[i]   # Specify the URL of the file to be downloaded
  
  # Specify the destination file path
  destfile_zip <- here(dir$rpg_data, paste0("tempfile_zip_", region_code_i, "_", year_i))
  dir.create(destfile_zip)
  destfile <- here(dir$rpg_data, paste0("tempfile_", region_code_i, "_", year_i))
  dir.create(destfile)
  
  options(timeout=800)
  # Download the file
  download.file(url_i, here(destfile_zip,"temp.001"), mode = "wb")
  
  # Print a message indicating the download is complete
  print(paste("File downloaded to", destfile))
  
  # Unzip file
  # archive(here(destfile_zip, "temp.001"))
  archive_extract(archive = here(destfile_zip,"temp.001"), dir = here(destfile))
  
  # Function to find the .shp file
  find_pg_shp_file <- function(directory) {
    shp_files <- list.files(destfile, pattern = "PARCELLES_GRAPHIQUES\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shp_files) > 0) {
      return(shp_files[1])  # Return the first matching .shp file
    } else {
      return(NULL)
    }
  }
  
  find_ia_shp_file <- function(directory) {
    shp_files <- list.files(destfile, pattern = "ILOTS_ANONYMES\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shp_files) > 0) {
      return(shp_files[1])  # Return the first matching .shp file
    } else {
      return(NULL)
    }
  }
  
  # Find the PARCELLES_GRAPHIQUES.shp file
  pg_shp_file <- find_pg_shp_file(destfile)
  # Find the PARCELLES_GRAPHIQUES.shp file
  ia_shp_file <- find_ia_shp_file(destfile)
  
  
  if (!is.null(pg_shp_file)) {
    print(paste("Shapefile 'PARCELLES_GRAPHIQUES' exists:", pg_shp_file))
    
    # Set the shapefile object name to be parcelles graphiques
    rpg_data <- pg_shp_file
    print(paste("Shapefile can be loaded:", rpg_data))
    
    # Additional processing can go here
  } else if (!is.null(ia_shp_file)){
    print(paste("Shapefile 'ILOTS_ANONYMES' is the only one:", ia_shp_file))
    
    # Set the shapefile object name to be ilots anonymes
    rpg_data <- ia_shp_file
    print(paste("Shapefile can be loaded:", rpg_data))
  } else {
    print("No shapefile found")
  }
  
  
  if (!is.null(rpg_data)) { # Apply to each RPG file (each url link, when it exists) the data preparation process
    
    if(!is.null(pg_shp_file)){
      rpg_map <- st_read(rpg_data)
      
      # Compute parcel area and add culture label 
      rpg_map_area <- rpg_map %>% 
        mutate(
          AREA_PARC = st_area(geometry), #To have it in m²
          year = year_i,
          region = region_i,
          CODE_GROUP = gsub("^0", "", CODE_GROUP)
        ) %>% 
        left_join(label_data, by = "CODE_GROUP")# Ajouter le libellé! Nom variable: NOM_CULTU
      
      # Open french communes shapefile (that is used to aggregate parcels at the commune level)
      contours <- st_read(here(dir$communes,"communes-20220101.shp")) %>% 
        mutate(geo_unit = insee)
      
      # Adapt CRS of geo_unit to match CRS of rpg data
      contours_rgf93 <- st_transform(contours, crs = st_crs(rpg_map_area))
      
      # Tell in which geo_unit falls each parcel
      rpg_joined <- st_join(st_make_valid(rpg_map_area), contours_rgf93, largest = T) # To test, use rpg_map_small to be quicker
      
      # Retain geo_unit with at least one parcel
      rpg_joined_filtered <- rpg_joined %>% 
        as.data.frame() %>% # Remove the sf structure
        dplyr::select(-geometry) %>% # Remove the geometry column
        filter(!is.na(ID_PARCEL))
      
      # Compute useful variable at the geo_unit level
      rpg_joined_gu <- rpg_joined_filtered %>%
        group_by(geo_unit) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the geo_unit in which it is located
        mutate(Surf_Agri_Tot = sum(AREA_PARC), # Surface of all agricultural parcels in the geo_unit
               N_Parcels      = n()
        ) %>% # Number of parcels in the geo_unit
        ungroup()
      
      result_i <- rpg_joined_gu %>% # Object that contain descriptive statistics
        group_by(geo_unit, CODE_GROUP) %>% # Aggregate by culture
        summarise(
          cult_label = first(LABEL_CODE_GROUP),
          data_type = "pg",
          region = first(region),
          name = first(nom),
          year = first(year),
          surf_tot_geo_unit_m2 = first(surf_ha)*10000,
          surf_agri_geo_unit_m2 = first(Surf_Agri_Tot),
          surf_cult_m2 = sum(AREA_PARC, na.rm = T),# Surface of parcels dedicated to each culture in each department
          surf_cult_perc = sum(AREA_PARC, na.rm = T) / Surf_Agri_Tot[1], # Percentage in the total agricultural area of the commune
          parcel_cult_n  = n(),
          parcel_cult_perc = parcel_cult_n / N_Parcels[1]) 
      
    } else if (!is.null(ia_shp_file)){
      rpg_map <- st_read(rpg_data)
      
      # Compute parcel area and add culture label 
      rpg_map_area <- rpg_map %>% 
        mutate(
          AREA_PARC = st_area(geometry),#To have it in m²
          CODE_GROUP = gsub("^0", "", as.character(CODE_CULTU)), # To homegenize names
          year = year_i,
          region = region_i
        ) %>% 
        # dplyr::select(-CODE_CULTU) %>% 
        left_join(label_data, by = "CODE_GROUP")# Ajouter le libellé! Nom variable: NOM_CULTU
      
      # Open french communes shapefile (that is used to aggregate parcels at the commune level)
      contours <- st_read(here(dir$communes,"communes-20220101.shp")) %>% 
        mutate(geo_unit = insee)
      # Adapt CRS of contours to rpg data
      contours_rgf93 <- st_transform(contours, crs = st_crs(rpg_map_area))
      
      # Tell in which geo_unit falls each parcel
      rpg_joined <- st_join(st_make_valid(rpg_map_area), contours_rgf93, largest = T) # To test and be quicker, use rpg_map small

      
      # Retain geo_unit with at least one parcel
      rpg_joined_filtered <- rpg_joined %>% 
        as.data.frame() %>% # Remove the sf structure
        dplyr::select(-geometry) %>% # Remove the geometry column
        filter(!is.na(NUM_ILOT)) # Only keep geo_unit with at least one parcel
      
      # Compute useful variable at the geo-unit level
      rpg_joined_gu <- rpg_joined_filtered %>%
        group_by(geo_unit) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the department in which it is located
        mutate(Surf_Agri_Tot = sum(AREA_PARC), # Surface of all agricultural parcels in the geo_unit
               N_Parcels      = n()
        ) %>% # Number of parcels in the geo_unit
        ungroup()
      
      # Finally, create object that aggregates by culture and by geo_unit
      result_i <- rpg_joined_gu %>% # Object 
        group_by(geo_unit, CODE_GROUP) %>% # Aggregate by culture and by geo_unit
        summarise(cult_label = first(LABEL_CODE_GROUP),
                  data_type = "ia",
                  region = first(region),
                  name = first(nom),
                  year = first(year),
                  surf_tot_geo_unit_m2 = first(surf_ha)*10000,
                  surf_agri_geo_unit_m2 = first(Surf_Agri_Tot),
                  surf_cult_m2 = sum(AREA_PARC, na.rm = T),# Surface of parcels dedicated to each culture in each department
                  surf_cult_perc = sum(AREA_PARC, na.rm = T) / Surf_Agri_Tot[1], # Percentage in the total agricultural area of the commune
                  parcel_cult_n  = n(),
                  parcel_cult_perc = parcel_cult_n / N_Parcels[1]
        ) 
    }
    
    # result_i
    
  } else {
    print(paste("The region", region_i, " does not have shapefile for year ", year_i))
    result_i <- NULL
    
  }
  
  # Delete file
  if (dir.exists(here(destfile_zip))) {
    #Delete file if it exists
    unlink(here(destfile_zip), recursive = T)
  }
  if (dir.exists(here(destfile))) {
    #Delete file if it exists
    unlink(here(destfile), recursive=TRUE)
  }
  return(result_i)
}

save(PrepareRPGData, file = here(dir$prep_data, "PrepareData_RFunction.Rdata"))
