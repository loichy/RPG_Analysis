#===============================================================================
# Description: Master RCode that compiles all RPG shapefiles and aggregate parcel
# data at the department or communal level
# author: Loic Henry and El Ghali Debbagh
# Contact loic.henry@dauphine.psl.eu
#===============================================================================

# Important note: to run this code, open first the R project "RPGAnalysis.Rproj"

##### Clean memory
rm(list=ls())
gc()

##### Only required package to initiate
# Clean and load packages (other packages are install and loaded from "00_PrepareEnvironment.R")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)


#####  Precised relative access to useful Rcodes
dir <- list() # First create an empty list, that we then fill with subobjects
dir$root <- getwd() # First: the place of the R project
dir$rcode <- paste0(dir$root, "/RCode")

##### Prepare working environment
source(here(dir$rcode, "00_PrepareEnvironment.R"))

##### Extract all RPG data files
source(here(dir$rcode, "01_RPGFileNames.R")) 
# cREATE THE ALL_RPG_LINKS OBJECT WHICH CONTAINS ALL URL LINKS TO DOWNLOAD DATA, AND THE CORRESPONDING REGION NAME AND YEAR

##### Load, prepare and append all rpg shapefiles

# Open french communes shapefile (that is used to aggregate parcels at the commune level)
contours <- st_read(here(dir$communes,"communes-20220101.shp")) %>% 
  mutate(geo_unit = insee)

# Load culture labels
libelle <- read.csv(file = here(dir$rpg_documentation,"REF_CULTURES_GROUPES_CULTURES_2020.csv"), sep = ";") # Load libelle data
label_data <- libelle %>% 
  mutate(CODE_GROUP = as.character(CODE_GROUPE_CULTURE),
         LABEL_CODE_GROUP = LIBELLE_GROUPE_CULTURE) %>% 
  dplyr::select(CODE_GROUP, LABEL_CODE_GROUP) %>% # Keep relevant variables
  group_by(CODE_GROUP) %>% # Keep single observation by group
  slice(1) %>% 
  ungroup() %>% 
  bind_rows(data.frame(CODE_GROUP = c("10", "12", "13", "27"),
                       LABEL_CODE_GROUP = c("Semences",
                                            "Gel industriel",
                                            "Autre gel",
                                            "Arboriculture")))

# Set download timeout
options(timeout=300)

# 1. ouvrir dans l'objet destfile les données avec le nom "parcelles grpahiques", sinon avec "ilot parcellaire"
# 2. Dans le cas de ilot parcellaire, regarder  si le code "load RPG" fonctionne (noms des variables similaires entre parcelles graphiques et ilot parecellaires?")
# Ajhouter dans le tableau issu du code "RPG Load data",  "corse_culture_bydep" (qu'il faudra renommer culture_bydep") ajouter le nom de la région
# Même chose avec l'année
# Rendre le code 02_Load_RPGFiles nettoyer des références à la corse
# Objectif: pouvoir le faire pour 3 éléments diofférents des liens de tlééchargement (1, 90 et 182)

# La boucle :
system.time(
results <- lapply(X = seq_along(all_rpg_links$url)[c(13,91,182)], FUN = function(i){
  # for(i in c(1,82,182)){ #c(1:length(all_rpg_links$url))
  
  print(paste("Iteration ", i, " for region ",all_rpg_links$region_name[i], " and for year ", all_rpg_links$year[i]))
  
  # i <- 13
  # i <- 82
  # i <- 182
  
  # Create objects useful for iteration i
  region_i <- all_rpg_links$region_name[i]
  year_i <- all_rpg_links$year[i]
  url_i <- all_rpg_links$url[i]   # Specify the URL of the file to be downloaded
  
  # Prepare folders for downloading
  tf <- tempfile()
  # Specify the destination file path
  destfile_zip <- here(dir$rpg_data, "temp_zip")
  dir.create(destfile_zip)
  destfile <- here(dir$rpg_data, "tempfile")
  dir.create(destfile)
  
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
          region = region_i
        ) %>% 
        left_join(label_data, by = "CODE_GROUP")# Ajouter le libellé! Nom variable: NOM_CULTU
      
      # Adapt CRS of geo_unit to match CRS of rpg data
      contours_rgf93 <- st_transform(contours, crs = st_crs(rpg_map_area))
      
      # Tell in which geo_unit falls each parcel
      rpg_joined <- st_join(rpg_map_area, contours_rgf93, largest = T) # To test, use rpg_map_small to be quicker
      
      # plottest <- ggplot() +
      #   geom_sf(data=rpg_joined,
      #           mapping = aes(fill=nom),
      #           lwd = 0) +
      #   geom_sf(data = contours_rgf93,
      #           aes(geometry = geometry),
      #           fill=NA) +
      #   coord_sf(crs=st_crs('+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'),
      #            xlim = c(573567.302074, 755250.729560),
      #            ylim = c(6773621.222320, 6905868.489852),
      #            expand = FALSE) +
      #   theme(legend.position = "none")
      # ggsave(filename = here(dir$figures,"plottest2.pdf"), width = 16, height =12)
      
      # Retain geo_unit with at least one parcel
      rpg_joined_filtered <- rpg_joined %>% 
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
          CODE_GROUP = as.character(CODE_CULTU), # To homegenize names
          year = year_i,
          region = region_i
        ) %>% 
        dplyr::select(-CODE_CULTU) %>% 
        left_join(label_data, by = "CODE_GROUP")# Ajouter le libellé! Nom variable: NOM_CULTU
      
      # Adapt CRS of contours to rpg data
      contours_rgf93 <- st_transform(contours, crs = st_crs(rpg_map_area))
      
      # Tell in which geo_unit falls each parcel
      rpg_joined <- st_join(rpg_map_area, contours_rgf93, largest = T) # To test, use rpg_map small
      
      # plottest <- ggplot() +
      #   geom_sf(data=rpg_joined, 
      #           mapping = aes(fill=nom), 
      #           lwd = 0) +
      #   geom_sf(data = contours_rgf93, 
      #           aes(geometry = geometry), 
      #           fill=NA) +
      #   coord_sf(crs=st_crs('+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'),
      #            xlim = c(573567.302074, 755250.729560), 
      #            ylim = c(6773621.222320, 6905868.489852), 
      #            expand = FALSE) +
      #   theme(legend.position = "none") 
      # ggsave(filename = here(dir$figures,"plottest.pdf"), width = 16, height =12) 
      
      # Retain geo_unit with at least one parcel
      rpg_joined_filtered <- rpg_joined %>% 
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
    
    names(result_i) <- paste(region_i, year_i, sep="_")
    # result_i
    
  } else {
    print(paste("The region", region_i, " does not have shapefile for year ", year_i))
    result_i <- NULL
    names(result_i) <- paste(region_i, year_i, sep="_")
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
  result_i
}
)
)

str(results)
